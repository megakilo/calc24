use std::sync::Arc;

use calc24;
use futures::future;
use rand::Rng;
use tokio::task;

async fn run(calculator: Arc<calc24::Calc24>, nums: [i32; 4]) {
    match calculator.calc(nums) {
        None => println!("{:?} -> No Solution", nums),
        Some(answer) => println!("{:?} -> {}", nums, answer),
    }
}

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    let mut rng = rand::thread_rng();
    const N: usize = 4;

    let calculator: Arc<calc24::Calc24> = Arc::new(calc24::Calc24::new(N));
    let mut nums = [0; N];
    let handles = (0..100000)
        .map(|_| {
            (0..N).for_each(|i| nums[i] = rng.gen_range(1..=13));
            return task::spawn(run(calculator.clone(), nums));
        })
        .collect::<Vec<task::JoinHandle<()>>>();
    future::join_all(handles).await;
}
