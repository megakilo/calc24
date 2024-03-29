use std::sync::Arc;

pub enum OpType {
    Add,
    Substract,
    Multiply,
    Divide,
}

pub enum Operand {
    Number(usize),
    Expression {
        op: OpType,
        left: Arc<Operand>,
        right: Arc<Operand>,
    },
}

impl Operand {
    fn eval(&self, nums: &[i32]) -> f64 {
        match self {
            Operand::Number(index) => nums[*index] as f64,
            Operand::Expression { op, left, right } => match op {
                OpType::Add => left.eval(nums) + right.eval(nums),
                OpType::Substract => left.eval(nums) - right.eval(nums),
                OpType::Multiply => left.eval(nums) * right.eval(nums),
                OpType::Divide => left.eval(nums) / right.eval(nums),
            },
        }
    }

    fn to_string(&self, nums: &[i32]) -> String {
        match self {
            Operand::Number(index) => nums[*index].to_string(),
            Operand::Expression { op, left, right } => match op {
                OpType::Add => format!("{} + {}", left.to_string(nums), right.to_string(nums)),
                OpType::Substract => format!(
                    "{} - {}",
                    left.to_string(nums),
                    right.to_string_helper(nums, false)
                ),
                OpType::Multiply => format!(
                    "{} * {}",
                    left.to_string_helper(nums, false),
                    right.to_string_helper(nums, false)
                ),
                OpType::Divide => format!(
                    "{} / {}",
                    left.to_string_helper(nums, false),
                    right.to_string_helper(nums, true)
                ),
            },
        }
    }

    fn to_string_helper(&self, nums: &[i32], is_denominator: bool) -> String {
        match self {
            Operand::Number(index) => nums[*index].to_string(),
            Operand::Expression { op, .. } => match op {
                OpType::Add | OpType::Substract => format!("({})", self.to_string(nums)),
                _ => {
                    if is_denominator {
                        format!("({})", self.to_string(nums))
                    } else {
                        self.to_string(nums)
                    }
                }
            },
        }
    }
}

fn combine(num1: &Arc<Operand>, num2: &Arc<Operand>) -> [Arc<Operand>; 6] {
    [
        Arc::new(Operand::Expression {
            op: OpType::Add,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Arc::new(Operand::Expression {
            op: OpType::Multiply,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Arc::new(Operand::Expression {
            op: OpType::Substract,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Arc::new(Operand::Expression {
            op: OpType::Substract,
            left: num2.clone(),
            right: num1.clone(),
        }),
        Arc::new(Operand::Expression {
            op: OpType::Divide,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Arc::new(Operand::Expression {
            op: OpType::Divide,
            left: num2.clone(),
            right: num1.clone(),
        }),
    ]
}

fn generate_expressions(nums: &[Arc<Operand>]) -> Vec<Arc<Operand>> {
    if nums.len() == 1 {
        return nums.to_vec();
    }
    let mut result = Vec::new();
    let n = nums.len();
    for i in 0..n {
        for j in (i + 1)..n {
            let mut reduced: Vec<Arc<Operand>> = nums
                .iter()
                .enumerate()
                .filter(|&(k, _)| k != i && k != j)
                .map(|(_, num)| num.clone())
                .collect();
            for num in combine(&nums[i], &nums[j]) {
                reduced.push(num);
                let mut r = generate_expressions(&reduced);
                result.append(&mut r);
                reduced.pop();
            }
        }
    }
    result
}

pub struct Calc24<const N: usize> {
    expressions: Vec<Arc<Operand>>,
}

impl<const N: usize> Calc24<N> {
    pub fn new() -> Calc24<N> {
        let indexes = (0..N)
            .map(|i| Arc::new(Operand::Number(i)))
            .collect::<Vec<Arc<Operand>>>();
        let expressions = generate_expressions(&indexes);
        Calc24 { expressions }
    }

    pub fn calc(&self, nums: [i32; N]) -> Option<String> {
        for e in &self.expressions {
            if e.eval(&nums) == 24_f64 {
                return Some(e.to_string(&nums));
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_3_numbers() {
        let calculator = Calc24::<3>::new();
        let result = calculator.calc([2, 3, 4]);
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_invalid_3_numbers() {
        let calculator = Calc24::<3>::new();
        let result = calculator.calc([1, 3, 4]);
        assert_eq!(result.is_none(), true);
    }

    #[test]
    fn test_basic_4_numbers() {
        let calculator = Calc24::<4>::new();
        let result = calculator.calc([1, 2, 3, 4]);
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_fraction_4_numbers() {
        let calculator = Calc24::<4>::new();
        let result = calculator.calc([3, 3, 7, 7]);
        assert_eq!(result.is_some(), true);
    }
}
