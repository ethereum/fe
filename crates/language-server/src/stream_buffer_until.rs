use futures::stream::Stream;
use futures::stream::{iter, Iter};
use log::info;
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

impl<'s, I, T, U> BufferUntilStream<I, T, U>
where
    I: Stream<Item = U>,
    T: Stream,
{
    pub fn new(input_stream: I, trigger_stream: T) -> Self {
        BufferUntilStream {
            input_stream,
            trigger_stream,
            pending_buffer: VecDeque::new(),
            ready_buffer: VecDeque::new(),
        }
    }

    // pub fn input_stream_mut(&mut self) -> &mut I {
    //     &mut self.input_stream
    // }

    // pub fn input_stream(&self) -> &I {
    //     &self.input_stream
    // }

    // pub fn trigger_stream_mut(&mut self) -> &mut T {
    //     &mut self.trigger_stream
    // }

    // pub fn trigger_stream(&self) -> &T {
    //     &self.trigger_stream
    // }
}
impl<I, T, U: Debug> Stream for BufferUntilStream<I, T, U>
where
    I: Stream<Item = U>,
    T: Stream,
{
    type Item = Iter<std::collections::vec_deque::IntoIter<U>>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let mut this = self.project();
        let ready_buffer: &mut VecDeque<U> = this.ready_buffer;
        let pending_buffer: &mut VecDeque<U> = this.pending_buffer;

        let mut finished = false;

        // Check if the input_stream has a new value
        while let Poll::Ready(Some(item)) = this.input_stream.as_mut().poll_next(cx) {
            info!("Received item from input_stream: {:?}", item);
            pending_buffer.push_back(item);
        }

        if let Poll::Ready(None) = this.input_stream.as_mut().poll_next(cx) {
            info!("input_stream finished");
            finished = true;
        }

        match this.trigger_stream.as_mut().poll_next(cx) {
            Poll::Ready(Some(_)) => {
                info!("Triggered, moving pending_buffer to ready_buffer");
                ready_buffer.append(pending_buffer);
            }
            Poll::Ready(None) => {
                ready_buffer.append(pending_buffer);
            }
            _ => {
                finished = true;
            }
        }

        // Send any ready buffer or finish up
        if !ready_buffer.is_empty() {
            info!("Returning items stream from ready_buffer");
            let current_ready_buffer = std::mem::take(this.ready_buffer);
            Poll::Ready(Some(iter(current_ready_buffer)))
        } else if finished {
            return Poll::Ready(None);
        } else {
            Poll::Pending
        }
    }
}

pub trait BufferUntilStreamExt<I, T, U: Debug>: Sized
where
    I: Stream<Item = U>,
    T: Stream,
{
    fn buffer_until(self, trigger: T) -> BufferUntilStream<I, T, U>;
}

impl<'s, I, T, U: Debug> BufferUntilStreamExt<I, T, U> for I
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
        )
        .flatten();

        input_sender.send(1).unwrap();
        input_sender.send(2).unwrap();
        input_sender.send(3).unwrap();

        while let Some(item) = accumulating_stream.next().now_or_never().flatten() {
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

        drop(trigger_sender);

        while let Some(item) = accumulating_stream.next().now_or_never().flatten() {
            output.push(item);
        }
        assert_eq!(output, vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    }

    // TODO: write tests for end of input stream
}
