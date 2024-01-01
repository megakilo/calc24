use std::sync::Arc;

use calc24;
use rand::Rng;
use tokio::sync::mpsc;
use tokio::task;

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    let mut rng = rand::thread_rng();
    const N: usize = 4;
    const TOTAL: usize = 100_000;

    let calculator = Arc::new(calc24::Calc24::<N>::new());
    let mut nums = [0; N];
    let (tx, mut rx) = mpsc::channel::<String>(100);
    (0..TOTAL).for_each(|_| {
        (0..N).for_each(|i| nums[i] = rng.gen_range(1..=13));
        let calculator = calculator.clone();
        let tx = tx.clone();
        task::spawn(async move {
            let answer = match calculator.calc(nums) {
                None => format!("{:?} -> No Solution", nums),
                Some(r) => format!("{:?} -> {}", nums, r),
            };
            let _ = tx.send(answer).await;
        });
    });
    for _ in 0..TOTAL {
        match rx.recv().await {
            None => println!("Unexpected"),
            Some(answer) => println!("{}", answer),
        };
    }
}
