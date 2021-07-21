use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    const N: usize = 4;

    let calculator = calc24::Calc24::new(N);

    let mut nums = [0 as f64; N];
    for _ in 0..1000 {
        let mut challenge = String::new();
        for j in 0..N {
            let x = rng.gen_range(1..=13);
            nums[j] = x as f64;
            if j > 0 {
                challenge.push_str(", ")
            }
            challenge.push_str(&x.to_string())
        }
        challenge.push_str(" -> ");
        if let Some(solution) = calculator.calc(&nums) {
            challenge.push_str(&solution);
        } else {
            challenge.push_str("No Solution");
        }
        println!("{}", &challenge);
    }
}
