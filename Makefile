#
# Building Chisel examples without too much sbt/scala/... stuff
#
# sbt looks for default into a folder ./project and . for build.sdt and Build.scala
# sbt creates per default a ./target folder

SBT = ~/2_safak_tez/sbt/sbt
#SBT = sbt


# Generate Verilog code

#generate:
#	$(SBT) "runMain simple.AluMain"

# Generate the C++ simulation and run the tests

test:
	$(SBT) test

build:
	$(SBT) compile

#fifo-view:
#	gtkwave generated/simple.FifoTester823761309/BubbleFifo.vcd --save=bubble.gtkw

#fifo: fifo-test fifo-view

# clean everything (including IntelliJ project settings)

clean:
	git clean -fd
