use std::sync::Arc;

use calc24;
use rand::Rng;
use tokio::task;

#[tokio::main(flavor = "multi_thread")]
async fn main() {
    let mut rng = rand::thread_rng();
    const N: usize = 4;
    const TOTAL: usize = 100_000;

    let calculator = Arc::new(calc24::Calc24::<N>::new());
    let mut nums = [0; N];
    let handles: Vec<task::JoinHandle<String>> = (0..TOTAL)
        .map(|_| {
            (0..N).for_each(|i| nums[i] = rng.gen_range(1..=13));
            let calculator = calculator.clone();
            task::spawn(async move {
                match calculator.calc(nums) {
                    None => format!("{:?} -> No Solution", nums),
                    Some(r) => format!("{:?} -> {}", nums, r),
                }
            })
        })
        .collect();
    for h in handles {
        println!("{}", h.await.unwrap());
    }
}
