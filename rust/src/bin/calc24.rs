use calc24;
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    const N: usize = 4;

    let calculator = calc24::Calc24::new(N);

    let mut nums = [0; N];
    for _ in 0..100000 {
        for j in 0..N {
            let x = rng.gen_range(1..=13);
            nums[j] = x;
        }
        match calculator.calc(nums) {
            None => println!("{:?} -> No Solution", &nums),
            Some(answer) => println!("{:?} -> {}", &nums, answer),
        }
    }
}
