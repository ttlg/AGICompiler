int iterate(int cx, int cy, int max, int thresh, int scale) {
    int zr = 0;
    int zi = 0;
    int i = 0;
    while (i < max && zr * zr + zi * zi <= thresh) {
        int tr = (zr * zr - zi * zi) / scale + cx;
        int ti = 2 * zr * zi / scale + cy;
        zr = tr;
        zi = ti;
        i = i + 1;
    }
    return i;
}

int main() {
    int S = 256;
    int T = 4 * S * S;
    int max = 40;
    int xmin = -512;
    int ymax = 300;
    int W = 76;
    int H = 30;
    int dx = 10;
    int dy = 20;

    int pal[10];
    pal[0] = 32;
    pal[1] = 46;
    pal[2] = 44;
    pal[3] = 58;
    pal[4] = 59;
    pal[5] = 61;
    pal[6] = 43;
    pal[7] = 42;
    pal[8] = 37;
    pal[9] = 64;

    for (int r = 0; r < H; r = r + 1) {
        int cy = ymax - r * dy;
        for (int c = 0; c < W; c = c + 1) {
            int cx = xmin + c * dx;
            int n = iterate(cx, cy, max, T, S);
            if (n >= max) {
                putchar(35);
            } else {
                int idx = n * 10 / max;
                if (idx > 9) { idx = 9; }
                putchar(pal[idx]);
            }
        }
        putchar(10);
    }
    return 0;
}
