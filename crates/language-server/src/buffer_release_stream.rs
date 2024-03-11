use std::{
    collections::VecDeque, pin::{pin, Pin}, task::{Context, Poll}
};

use futures::stream::Stream;
use futures::stream::StreamExt;
use tokio::sync::mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender};

use pin_project::pin_project;

#[pin_project]
struct AccumulatingStream<I, T, U> {
    #[pin]
    input_stream: I,
    #[pin]
    trigger_stream: T,
    // #[pin]
    pending_buffer: VecDeque<U>,
    // #[pin]
    ready_buffer: VecDeque<U>
}

impl<I, T, U> AccumulatingStream<I, T, U>
where
    I: Stream<Item = U>,
    T: Stream,
{
    fn new(input_stream: I, trigger_stream: T) -> Self {
        AccumulatingStream {
            input_stream,
            trigger_stream,
            pending_buffer: VecDeque::new(),
            ready_buffer: VecDeque::new(),
        }
    }
}
impl<I, T, U> Stream for AccumulatingStream<I, T, U>
where
    I: Stream<Item = U>,
    T: Stream,
{
    type Item = I::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();

        let ready_buffer : &mut VecDeque<U> = this.ready_buffer;
        // Drain the existing accumulated values
        if let Some(item) = ready_buffer.pop_front() {
            return Poll::Ready(Some(item));
        }

        // // Check if the trigger_stream has a new value
        if let Poll::Ready(Some(_)) = this.trigger_stream.poll_next(cx) {
            // move the pending buffer to the ready buffer
            let pending_buffer : &mut VecDeque<U> = this.pending_buffer;
            let ready_buffer : &mut VecDeque<U> = this.ready_buffer;
            ready_buffer.append(pending_buffer);
        }

        // Check if the input_stream has a new value
        if let Poll::Ready(Some(item)) = this.input_stream.poll_next(cx) {
            let pending_buffer : &mut VecDeque<U> = this.pending_buffer;
            pending_buffer.push_back(item);
        }

        Poll::Pending
    }
}

// impl<I, T, U> Stream for AccumulatingStream<I, T, U>
// where
//     I: Stream<Item = U>,
//     T: Stream,
// {
//     type Item = I::Item;

//     fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
//         let this = self.project();

//         let trigger_stream: Pin<&mut T> = this.trigger_stream;
//         // let accumulation_receiver : Pin<&mut UnboundedReceiver<U>> = this.accumulation_receiver;
//         let output_sender: Pin<&mut UnboundedSender<UnboundedReceiver<U>>> = this.output_sender;
//         if let Poll::Ready(Some(_)) = trigger_stream.poll_next(cx) {
//             let (new_accumulation_sender, new_accumulation_receiver) = unbounded_channel();
//             let old_accumulation_receiver =
//                 std::mem::replace(this.accumulation_receiver, new_accumulation_receiver);
//             // we also need to replace the accumulation sender but it's pinned
//             let _ = std::mem::replace(this.accumulation_sender, new_accumulation_sender);
//             let _ = output_sender.send(old_accumulation_receiver);
//         }

//         let mut output_receiver: Pin<&mut UnboundedReceiver<UnboundedReceiver<U>>> =
//             this.output_receiver;
//         if let Poll::Ready(Some(mut inner)) = output_receiver.poll_recv(cx) {
//             if let Poll::Ready(Some(item)) = inner.poll_recv(cx) {
//                 return Poll::Ready(Some(item));
//             }
//         }

//         let input_stream: Pin<&mut I> = this.input_stream;
//         // let accumulation_sender: Pin<&mut UnboundedSender<U>> = self.accumulation_sender;
//         if let Poll::Ready(Some(item)) = input_stream.poll_next(cx) {
//             let _ = this.accumulation_sender.send(item);
//         }

//         Poll::Pending
//     }
// }

// how about some tests
#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use fork_stream::StreamExt as _;
    use tokio::sync::Mutex;

    use super::*;
    use futures::stream::StreamExt;
    use tokio::time::{timeout, Duration};
    use tokio_stream::wrappers::{BroadcastStream, UnboundedReceiverStream};

    #[tokio::test]
    async fn test_accumulating_stream() {
        let (trigger_sender, mut trigger_receiver) = tokio::sync::broadcast::channel(100);
        let (input_sender, input_receiver) = unbounded_channel();
        // let (output_sender, mut output_receiver) = tokio::sync::broadcast::channel(1);

        let mut output = vec![];

        let mut accumulating_stream = AccumulatingStream::new(
            UnboundedReceiverStream::from(input_receiver),
            BroadcastStream::from(trigger_sender.subscribe()),
        );

        input_sender.send(1).unwrap();
        input_sender.send(2).unwrap();
        input_sender.send(3).unwrap();
        trigger_sender.send(()).unwrap();
        // tokio::time::sleep(Duration::from_millis(1000)).await;

        while let Some(item) = accumulating_stream.next().await {
            output.push(item);
        }

        assert_eq!(output, vec![1, 2, 3]);

        input_sender.send(4).unwrap();
        input_sender.send(5).unwrap();
        input_sender.send(6).unwrap();
        trigger_sender.send(()).unwrap();
        input_sender.send(7).unwrap();
        input_sender.send(8).unwrap();
        input_sender.send(9).unwrap();
        input_sender.send(10).unwrap();
    }
}
