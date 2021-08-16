use std::rc::Rc;

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
        left: Rc<Operand>,
        right: Rc<Operand>,
    },
}

impl Operand {
    fn eval(&self, nums: &[f64]) -> f64 {
        match self {
            Operand::Number(index) => nums[*index],
            Operand::Expression { op, left, right } => match op {
                OpType::Add => left.eval(nums) + right.eval(nums),
                OpType::Substract => left.eval(nums) - right.eval(nums),
                OpType::Multiply => left.eval(nums) * right.eval(nums),
                OpType::Divide => left.eval(nums) / right.eval(nums),
            },
        }
    }

    fn to_string(&self, nums: &[f64]) -> String {
        match self {
            Operand::Number(index) => nums[*index].to_string(),
            Operand::Expression { op, left, right } => match op {
                OpType::Add => format!("{} + {}", left.to_string(nums), right.to_string(nums)),
                OpType::Substract => format!(
                    "{} - {}",
                    left.to_string(nums),
                    Operand::to_string_helper(right, nums, false)
                ),
                OpType::Multiply => format!(
                    "{} * {}",
                    Operand::to_string_helper(left, nums, false),
                    Operand::to_string_helper(right, nums, false),
                ),
                OpType::Divide => format!(
                    "{} / {}",
                    Operand::to_string_helper(left, nums, false),
                    Operand::to_string_helper(right, nums, true),
                ),
            },
        }
    }

    fn to_string_helper(node: &Operand, nums: &[f64], is_denominator: bool) -> String {
        match node {
            Operand::Number(index) => nums[*index].to_string(),
            Operand::Expression { op, .. } => match op {
                OpType::Add | OpType::Substract => format!("({})", node.to_string(nums)),
                _ => {
                    if is_denominator {
                        format!("({})", node.to_string(nums))
                    } else {
                        node.to_string(nums)
                    }
                }
            },
        }
    }
}

fn combine(num1: &Rc<Operand>, num2: &Rc<Operand>) -> [Rc<Operand>; 6] {
    [
        Rc::new(Operand::Expression {
            op: OpType::Add,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Rc::new(Operand::Expression {
            op: OpType::Multiply,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Rc::new(Operand::Expression {
            op: OpType::Substract,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Rc::new(Operand::Expression {
            op: OpType::Substract,
            left: num2.clone(),
            right: num1.clone(),
        }),
        Rc::new(Operand::Expression {
            op: OpType::Divide,
            left: num1.clone(),
            right: num2.clone(),
        }),
        Rc::new(Operand::Expression {
            op: OpType::Divide,
            left: num2.clone(),
            right: num1.clone(),
        }),
    ]
}

fn generate_expressions(nums: &[Rc<Operand>]) -> Vec<Rc<Operand>> {
    if nums.len() == 1 {
        return nums.to_vec();
    }
    let mut result = Vec::new();
    let n = nums.len();
    for i in 0..n {
        for j in (i + 1)..n {
            let mut reduced: Vec<Rc<Operand>> = nums
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

pub struct Calc24 {
    expressions: Vec<Rc<Operand>>,
}

impl Calc24 {
    pub fn new(n: usize) -> Calc24 {
        let indexes = (0..n)
            .map(|i| Rc::new(Operand::Number(i)))
            .collect::<Vec<Rc<Operand>>>();
        let expressions = generate_expressions(&indexes);
        Calc24 { expressions }
    }

    pub fn calc(&self, nums: &[f64]) -> Option<String> {
        for e in &self.expressions {
            if e.eval(nums) == 24_f64 {
                return Some(e.to_string(nums));
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
        let calculator = Calc24::new(3);
        let result = calculator.calc(&[2f64, 3f64, 4f64]);
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_invalid_3_numbers() {
        let calculator = Calc24::new(3);
        let result = calculator.calc(&[1f64, 3f64, 4f64]);
        assert_eq!(result.is_none(), true);
    }

    #[test]
    fn test_basic_4_numbers() {
        let calculator = Calc24::new(4);
        let result = calculator.calc(&[1f64, 2f64, 3f64, 4f64]);
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_fraction_4_numbers() {
        let calculator = Calc24::new(4);
        let result = calculator.calc(&[3f64, 3f64, 7f64, 7f64]);
        assert_eq!(result.is_some(), true);
    }
}
