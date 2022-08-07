
# buraya constant zero hash değeri gelecek
constZero_h = [0x7d, 0xf7, 0x6b, 0x0c, 0x1a, 0xb8, 0x99, 0xb3, 0x3e, 0x42, 0xf0, 0x47, 0xb9, 0x1b, 0x54, 0x6f]
K1 = []

msb = 0
for i in range(15,-1,-1):
    K1.insert(0, msb | ((constZero_h[i] << 1) & 0xFF))
    msb = (constZero_h[i] >> 7) & 1

msb = (constZero_h[0] >> 7) & 1
if msb == 1:
    K1[15] |= 87

print(",".join(hex(x) for x in K1))

#for x in K1:
#    print(hex(x))