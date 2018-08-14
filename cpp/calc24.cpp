#include <iostream>
#include <algorithm>
#include <random>
#include <vector>
#include <string>

using namespace std;
using number = pair<float, string>;
using parted = pair<vector<number>, vector<number>>;

vector<number> combine2(const vector<number>& nums) {
    vector<number> result;
    result.push_back(make_pair(nums[0].first + nums[1].first, "(" + nums[0].second + " + " + nums[1].second + ")"));
    result.push_back(make_pair(nums[0].first * nums[1].first, "(" + nums[0].second + " * " + nums[1].second + ")"));
    if (nums[0].first > nums[1].first) {
        result.push_back(make_pair(nums[0].first - nums[1].first, "(" + nums[0].second + " - " + nums[1].second + ")"));
    } else {
        result.push_back(make_pair(nums[1].first - nums[0].first, "(" + nums[1].second + " - " + nums[0].second + ")"));
    }
    if (nums[1].first != 0) {
        result.push_back(make_pair(nums[0].first / nums[1].first, "(" + nums[0].second + " / " + nums[1].second + ")"));
    }
    if (nums[0].first != 0) {
        result.push_back(make_pair(nums[1].first / nums[0].first, "(" + nums[1].second + " / " + nums[0].second + ")"));
    }
    return result;
}

vector<parted> split(const vector<number>& nums, int n) {
    vector<parted> result;
    if (n == 0) {
        result.push_back(make_pair(vector<number>(), nums));
        return result;
    }
    if (nums.size() <= n) {
        result.push_back(make_pair(nums, vector<number>()));
        return result;
    }
    for (parted p : split(vector<number>(nums.cbegin()+1, nums.cend()) , n)) {
        p.second.push_back(nums[0]);
        result.push_back(p);
    }
    for (parted p : split(vector<number>(nums.cbegin()+1, nums.cend()) , n - 1)) {
        p.first.push_back(nums[0]);
        result.push_back(p);
    }
    return result;
}

vector<vector<number>> reduce(const vector<number>& nums) {
    vector<vector<number>> result;
    for (parted& p : split(nums, 2)) {
        for (number x : combine2(p.first)) {
            vector<number> v(p.second);
            v.push_back(x);
            result.push_back(v);
        }
    }
    return result;
}

vector<number> calc(const vector<number>& nums) {
    if (nums.size() == 1) {
        return nums;
    }
    vector<number> result;
    for (vector<number> x : reduce(nums)) {
        vector<number> v = calc(x);
        result.insert(result.end(), v.begin(), v.end());
    }
    return result;
}

int main() {
    const int COUNT = 4;
    srand(time(NULL));
    for (int i = 0; i < 1000; i++) {
        vector<number> nums(COUNT);
        for (int j = 0; j < COUNT; j++) {
            int t = rand() % 13 + 1;
            nums[j] = make_pair(float(t), to_string(t));
        }
        string s = accumulate(nums.begin(), nums.end(), string(), [](string &ss, number &p) {
            return ss.empty() ? p.second : ss + ", " + p.second;
        });
        cout << s << " -> ";
        for (number& result : calc(nums)) {
            if (result.first == 24) {
                cout << result.second << endl;
                goto next;
            }
        }
        cout << "No Solution" << endl;
next:
        continue;
    }
    return 0;
}
