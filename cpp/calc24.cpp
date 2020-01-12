#include <algorithm>
#include <iostream>
#include <memory>
#include <optional>
#include <random>
#include <string>
#include <vector>

using namespace std;

enum class OpType { Unknown, Plus, Minus, Multiply, Divide };

struct Number {
  float value;
  string expr;
  OpType op;
  Number() {}
  Number(const float v, const string& e, const OpType o) : value(v), expr(e), op(o) {}
  Number(const Number& num1, const Number& num2, const OpType current_op) {
    op = current_op;
    switch (op) {
    case OpType::Plus:
      value = num1.value + num2.value;
      expr = num1.expr + " + " + num2.expr;
      break;
    case OpType::Minus:
      value = num1.value - num2.value;
      expr = num1.expr + " - " + create_expr(num2, false);
      break;
    case OpType::Multiply:
      value = num1.value * num2.value;
      expr = create_expr(num1, false) + " * " + create_expr(num2, false);
      break;
    case OpType::Divide:
      value = num1.value / num2.value;
      expr = create_expr(num1, false) + " * " + create_expr(num2, true);
      break;
    default:
      exit(1);
    }
  }

  inline string create_expr(const Number &num, const bool is_denominator) {
    if (num.op == OpType::Plus || num.op == OpType::Minus ||
        (is_denominator && num.op == OpType::Multiply))
      return "(" + num.expr + ")";
    return num.expr;
  }
};

vector<Number> combine(const Number& num1, const Number& num2) {
  vector<Number> result;
  result.emplace_back(num1, num2, OpType::Plus);
  result.emplace_back(num1, num2, OpType::Multiply);
  if (num1.value > num2.value) {
    result.emplace_back(num1, num2, OpType::Minus);
  } else {
    result.emplace_back(num2, num1, OpType::Minus);
  }
  if (num2.value != 0) {
    result.emplace_back(num1, num2, OpType::Divide);
  }
  if (num1.value != 0) {
    result.emplace_back(num2, num1, OpType::Divide);
  }
  return result;
}

optional<string> calc(const vector<Number>& nums, const float target) {
  if (nums.size() == 1) {
    if (nums[0].value == target) {
      return nums[0].expr;
    } else {
      return {};
    }
  }

  vector<Number> reduced;
  for (int i = 0; i < nums.size(); i++) {
    for (int j = i + 1; j < nums.size(); j++) {
      reduced.clear();
      for (int k = 0; k < nums.size(); k++) {
        if (k == i || k == j) continue;
        reduced.push_back(nums[k]);
      }
      for (const Number& num : combine(nums[i], nums[j])) {
        reduced.push_back(num);
        const auto result = calc(reduced, target);
        if (result.has_value()) {
          return result;
        }
        reduced.pop_back();
      }
    }
  }
  return {};
}

int main() {
  srand(time(NULL));
  vector<Number> nums(4);
  for (int i = 0; i < 1000; i++) {
    for (int j = 0; j < 4; j++) {
      int t = rand() % 13 + 1;
      nums[j] = Number(float(t), to_string(t), OpType::Unknown);
    }
    string challenge = accumulate(
        nums.begin(), nums.end(), string(), [](string& ss, Number& p) {
          return ss.empty() ? p.expr : ss + ", " + p.expr;
        });
    const auto result = calc(nums, 24);
    if (result.has_value()) {
      cout << challenge << " -> " << result.value() << endl;
    } else {
      cout << challenge << " -> No Solution" << endl;
    }
  }
  return 0;
}
