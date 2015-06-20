#include <iostream>
using namespace std;

const int d = 1000000;
int phi[d + 10];

int main() {
	for (int i = 2; i <= d; i++)phi[i] = i;
	for (int p = 2; p <= d; p++)if (phi[p] == p){
		for (int kp = p; kp <= d; kp += p) {
			phi[kp] -= phi[kp] / p;
		}
	}
	long long res = 0;
	for (int i = 2; i <= d; i++)res += phi[i];
	cout << res << endl;
	return 0;
}