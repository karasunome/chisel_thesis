import numpy as np

text_sec_start = 0x1000
text_sec_size = 0x1000
text_sec_end = text_sec_start + text_sec_size
test_loc = "/home/aselsan/2_safak_tez/chisel_thesis/src/test/scala/validator"
test_name = "InputElfTest.scala"
elf_loc = "/home/aselsan/2_safak_tez/riscv-tests/isa"
elf_name = "rv32mi-p-breakpoint"

testbench_file = open("file1.txt", "w+")

elf_content = np.fromfile(elf_loc+"/"+elf_name,  dtype=np.uint8)

print("Array(")
for i in range(text_sec_start,text_sec_end,16):
    print("Array(",end ="")
    print(",".join(hex(x) for x in elf_content[i:i+16]), end="")
    if i < text_sec_end-16:
        print("),", end="\n")
print("))")
