use futures::stream::Stream;
use std::{
    collections::VecDeque,
    fmt::Debug,
    pin::{pin, Pin},
    task::{Context, Poll},
};

use pin_project::pin_project;

#[pin_project(project_replace)]
pub struct BufferUntilStream<I, T, U> {
    #[pin]
    input_stream: I,
    #[pin]
    trigger_stream: T,
    pending_buffer: VecDeque<U>,
    ready_buffer: VecDeque<U>,
}

impl<I, T, U> BufferUntilStream<I, T, U>
where
    I: Stream<Item = U>,
    T: Stream,
{
    fn new(input_stream: I, trigger_stream: T) -> Self {
        BufferUntilStream {
            input_stream,
            trigger_stream,
            pending_buffer: VecDeque::new(),
            ready_buffer: VecDeque::new(),
        }
    }
}
impl<I, T, U: Debug> Stream for BufferUntilStream<I, T, U>
where
    I: Stream<Item = U>,
    T: Stream,
{
    type Item = I::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();
        let ready_buffer: &mut VecDeque<U> = this.ready_buffer;
        let pending_buffer: &mut VecDeque<U> = this.pending_buffer;

        // Check if the input_stream has a new value
        while let Poll::Ready(Some(item)) = this.input_stream.as_mut().poll_next(cx) {
            println!("Received item from input_stream: {:?}", item);
            pending_buffer.push_back(item);
        }

        // Drain the ready buffer
        if let Some(item) = ready_buffer.pop_front() {
            println!("Returning item from ready_buffer: {:?}", item);
            return Poll::Ready(Some(item));
        }

        // Check if the trigger_stream has a new value
        if let Poll::Ready(Some(_)) = this.trigger_stream.poll_next(cx) {
            // Move the pending buffer to the ready buffer
            println!("Triggered, moving pending_buffer to ready_buffer");
            ready_buffer.append(pending_buffer);
            println!("Ready buffer length after trigger: {}", ready_buffer.len());

            // Return the next item from the ready buffer
            if let Some(item) = ready_buffer.pop_front() {
                println!("Returning item from ready_buffer after trigger: {:?}", item);
                return Poll::Ready(Some(item));
            }
        }

        Poll::Pending
    }
}

pub trait BufferUntilStreamExt<I, T, U: Debug>: Sized
where
    I: Stream<Item = U>,
    T: Stream,
{
    fn buffer_until(self, trigger: T) -> BufferUntilStream<I, T, U>;
}

impl<I, T, U: Debug> BufferUntilStreamExt<I, T, U> for I
where
    I: Stream<Item = U>,
    T: Stream,
{
    fn buffer_until(self, trigger: T) -> BufferUntilStream<I, T, U> {
        BufferUntilStream::new(self, trigger)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use futures::{stream::StreamExt, FutureExt};
    use tokio_stream::wrappers::{BroadcastStream, UnboundedReceiverStream};

    #[tokio::test]
    async fn test_accumulating_stream() {
        println!("running test_accumulating_stream");
        let (trigger_sender, trigger_receiver) = tokio::sync::broadcast::channel(100);
        let (input_sender, input_receiver) = tokio::sync::mpsc::unbounded_channel();

        let mut output = vec![];

        let mut accumulating_stream = BufferUntilStream::new(
            UnboundedReceiverStream::from(input_receiver),
            BroadcastStream::from(trigger_receiver),
        );

        input_sender.send(1).unwrap();
        input_sender.send(2).unwrap();
        input_sender.send(3).unwrap();

        while let Some(item) = accumulating_stream.next().now_or_never().flatten()
        // timeout(Duration::from_millis(0), accumulating_stream.next()).await
        {
            output.push(item);
        }
        assert_eq!(output, Vec::<i32>::new());

        trigger_sender.send(()).unwrap();

        while let Some(item) = accumulating_stream.next().now_or_never().flatten() {
            output.push(item);
        }
        assert_eq!(output, vec![1, 2, 3]);

        input_sender.send(4).unwrap();
        input_sender.send(5).unwrap();
        input_sender.send(6).unwrap();
        while let Some(item) = accumulating_stream.next().now_or_never().flatten() {
            output.push(item);
        }

        assert_eq!(output, vec![1, 2, 3]);
        trigger_sender.send(()).unwrap();
        while let Some(item) = accumulating_stream.next().now_or_never().flatten() {
            output.push(item);
        }

        assert_eq!(output, vec![1, 2, 3, 4, 5, 6]);
        input_sender.send(7).unwrap();
        input_sender.send(8).unwrap();
        input_sender.send(9).unwrap();
        input_sender.send(10).unwrap();
        while let Some(item) = accumulating_stream.next().now_or_never().flatten() {
            output.push(item);
        }
        assert_eq!(output, vec![1, 2, 3, 4, 5, 6]);
    }
}
