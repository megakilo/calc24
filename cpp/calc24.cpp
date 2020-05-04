#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <vector>

enum class OpType { None, Plus, Minus, Multiply, Divide };

struct Number {
  float value;
  std::shared_ptr<Number> left;
  std::shared_ptr<Number> right;
  OpType op;
  Number(const float v) : value(v), op(OpType::None) {}
  Number(std::shared_ptr<Number> num1, std::shared_ptr<Number> num2,
         const OpType current_op) {
    op = current_op;
    left = num1;
    right = num2;
    switch (op) {
      case OpType::None:
        value = 0;
        break;
      case OpType::Plus:
        value = num1->value + num2->value;
        break;
      case OpType::Minus:
        value = num1->value - num2->value;
        break;
      case OpType::Multiply:
        value = num1->value * num2->value;
        break;
      case OpType::Divide:
        value = num1->value / num2->value;
        break;
    }
  }
};

std::string print(std::shared_ptr<Number> num);

std::string create_expr(std::shared_ptr<Number> num,
                        const bool is_denominator) {
  std::string s;
  if (num->op == OpType::Plus || num->op == OpType::Minus ||
      (is_denominator && num->op != OpType::None))
    return s.append("(").append(print(num)).append(")");
  return s.append(print(num));
}

std::string print(std::shared_ptr<Number> num) {
  std::string s;
  switch (num->op) {
    case OpType::None:
      s.append(std::to_string(int(num->value)));
      break;
    case OpType::Plus:
      s.append(print(num->left)).append(" + ").append(print(num->right));
      break;
    case OpType::Minus:
      s.append(print(num->left))
          .append(" - ")
          .append(create_expr(num->right, false));
      break;
    case OpType::Multiply:
      s.append(create_expr(num->left, false))
          .append(" * ")
          .append(create_expr(num->right, false));
      break;
    case OpType::Divide:
      s.append(create_expr(num->left, false))
          .append(" / ")
          .append(create_expr(num->right, true));
      break;
  }
  return s;
}

std::vector<std::shared_ptr<Number>> combine(std::shared_ptr<Number> num1,
                                             std::shared_ptr<Number> num2) {
  std::vector<std::shared_ptr<Number>> result;
  result.push_back(std::make_shared<Number>(num1, num2, OpType::Plus));
  result.push_back(std::make_shared<Number>(num1, num2, OpType::Multiply));
  if (num1->value > num2->value) {
    result.push_back(std::make_shared<Number>(num1, num2, OpType::Minus));
  } else {
    result.push_back(std::make_shared<Number>(num2, num1, OpType::Minus));
  }
  if (num2->value != 0) {
    result.push_back(std::make_shared<Number>(num1, num2, OpType::Divide));
  }
  if (num1->value != 0) {
    result.push_back(std::make_shared<Number>(num2, num1, OpType::Divide));
  }
  return result;
}

std::shared_ptr<Number> calc(const std::vector<std::shared_ptr<Number>>& nums,
                             const float target) {
  const int N = nums.size();
  if (N == 1) {
    if (nums[0]->value == target) {
      return nums[0];
    } else {
      return nullptr;
    }
  }

  std::vector<std::shared_ptr<Number>> reduced;
  for (int i = 0; i < N; i++) {
    for (int j = i + 1; j < N; j++) {
      reduced.clear();
      for (int k = 0; k < N; k++) {
        if (k == i || k == j) continue;
        reduced.push_back(nums[k]);
      }
      for (std::shared_ptr<Number> num : combine(nums[i], nums[j])) {
        reduced.push_back(num);
        const auto result = calc(reduced, target);
        if (result != nullptr) {
          return result;
        }
        reduced.pop_back();
      }
    }
  }
  return nullptr;
}

std::string calc24(const std::vector<int>& nums) {
  std::vector<std::shared_ptr<Number>> numbers;
  std::string challenge;
  for (int i = 0; i < nums.size(); i++) {
    int x = nums[i];
    numbers.emplace_back(std::make_shared<Number>(float(x)));
    if (i > 0) challenge.append(", ");
    challenge.append(std::to_string(x));
  }
  challenge.append(" -> ");
  const auto result = calc(numbers, 24);
  if (result != nullptr) {
    challenge.append(print(result));
  } else {
    challenge.append("No Solution");
  }
  return challenge;
}

int main() {
  srand(time(NULL));
  std::vector<int> nums;
  for (int i = 0; i < 1000; i++) {
    nums.clear();
    for (int j = 0; j < 4; j++) {
      nums.push_back(rand() % 13 + 1);
    }
    std::cout << calc24(nums) << std::endl;
  }
  return 0;
}
