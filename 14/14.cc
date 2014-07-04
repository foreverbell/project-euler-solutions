#include <bits/stdc++.h>

using namespace std;

map<long long, int> steps;

int f(long long x) {
	if (x == 1) return 1;
	if (steps.find(x) != steps.end()) return steps[x];
	int ret = 1;
	if (x % 2 == 0) {
		ret += f(x / 2);
	} else ret += f(3 * x + 1);
	steps[x] = ret;
	return ret;
}

int main() {
	int value = 0, mx = -1;
	for (int i = 1; i <= 1000000; ++i) {
		int tmp = f(i);
		if (tmp > value) {
			value = tmp;
			mx = i;
		}
	}
	cout << mx << endl;
	return 0;
}

