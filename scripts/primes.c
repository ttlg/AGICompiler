int print_num(int n) {
    if (n >= 10) print_num(n / 10);
    putchar(n % 10 + 48);
    return 0;
}

int print_aligned(int n) {
    if (n < 100) putchar(32);
    if (n < 10) putchar(32);
    print_num(n);
    return 0;
}

int main() {
    int N = 500;
    int sieve[500];

    for (int i = 0; i < N; i = i + 1) {
        sieve[i] = 0;
    }

    for (int i = 2; i * i < N; i = i + 1) {
        if (sieve[i] == 0) {
            for (int j = i * i; j < N; j = j + i) {
                sieve[j] = 1;
            }
        }
    }

    int count = 0;
    for (int i = 2; i < N; i = i + 1) {
        if (sieve[i] == 0) {
            print_aligned(i);
            count = count + 1;
            if (count % 15 == 0) {
                putchar(10);
            } else {
                putchar(32);
            }
        }
    }
    putchar(10);
    return count;
}
