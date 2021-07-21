extern crate calc24;

#[cfg(test)]
mod tests {
    #[test]
    fn test_basic_3_numbers() {
        let calculator = calc24::Calc24::new(3);
        let result = calculator.calc(&[2f64, 3f64, 4f64]);
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_invalid_3_numbers() {
        let calculator = calc24::Calc24::new(3);
        let result = calculator.calc(&[1f64, 3f64, 4f64]);
        assert_eq!(result.is_none(), true);
    }

    #[test]
    fn test_basic_4_numbers() {
        let calculator = calc24::Calc24::new(4);
        let result = calculator.calc(&[1f64, 2f64, 3f64, 4f64]);
        assert_eq!(result.is_some(), true);
    }

    #[test]
    fn test_fraction_4_numbers() {
        let calculator = calc24::Calc24::new(4);
        let result = calculator.calc(&[3f64, 3f64, 7f64, 7f64]);
        assert_eq!(result.is_some(), true);
    }
}
