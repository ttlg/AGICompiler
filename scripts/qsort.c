int print_num(int n) {
    if (n >= 10) {
        print_num(n / 10);
    }
    putchar(n % 10 + 48);
    return 0;
}

int print_arr(int *a, int len) {
    putchar(91);
    for (int i = 0; i < len; i = i + 1) {
        if (i > 0) {
            putchar(44);
            putchar(32);
        }
        print_num(*(a + i));
    }
    putchar(93);
    putchar(10);
    return 0;
}

int swap(int *a, int *b) {
    int tmp = *a;
    *a = *b;
    *b = tmp;
    return 0;
}

int partition(int *arr, int lo, int hi) {
    int pivot = *(arr + hi);
    int i = lo - 1;
    for (int j = lo; j < hi; j = j + 1) {
        if (*(arr + j) < pivot) {
            i = i + 1;
            swap(arr + i, arr + j);
        }
    }
    swap(arr + i + 1, arr + hi);
    return i + 1;
}

int qs(int *arr, int lo, int hi) {
    if (lo < hi) {
        int p = partition(arr, lo, hi);
        qs(arr, lo, p - 1);
        qs(arr, p + 1, hi);
    }
    return 0;
}

int main() {
    int a[8];
    a[0] = 38;
    a[1] = 27;
    a[2] = 43;
    a[3] = 3;
    a[4] = 9;
    a[5] = 82;
    a[6] = 10;
    a[7] = 1;
    print_arr(a, 8);
    qs(a, 0, 7);
    print_arr(a, 8);
    return 0;
}
