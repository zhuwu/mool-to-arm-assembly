##################################################
Read Me

Author: Zhu Wu, A0134429E
        Wang Peikai, A0123507N
##################################################

I. Compilation and Running
--------------------------------------------------
1. Generate compiler

Run make at this directory. The compiler is generated as "mOOL_compiler".

2. Compile MOOL code to ARM code

1) Non-optimized mode:

Run ./mOOL_compiler <directory of test program>. The generated ARM code will be written as "arm.s" in this directory.

2) Optimized mode:

Run ./mOOL_compiler o <directory of test program>. The generated ARM code will be written as "arm.s" in this directory.

3. Run ARM code in emulator shell

1) Start emulator.
2) Use arm-linux-androideabi-g++ to compile "arm.s" into "out".
3) Copy "out" into path "/data/local/tmp/" directory of emulator using adb.
4) In emulator shell, run "/data/local/tmp/out".

II. Test Cases
--------------------------------------------------
1. Hello world test: prints "Hello world" and prints results of some simple expression.
Directory: ./testcases/hello_world.txt

2. Sum 1 to 100 test: prints results of sum from 1 to 100.
Directory: ./testcases/sum_one_to_hundred.txt

3. Factorial test: prints factorial results.
Directory: ./testcases/factorial.txt

4. Print shape test: prints shape of rectangle by using "*".
Directory: ./testcases/print_shape.txt

5. Multiple classes & method more than 4 parameters test:
Directory: ./testcases/multiple_classes_and_multiple_params.txt

6. Code optimization test:
Directory: ./testcases/code_optimization.txt

7. Dead code elimination test:
Directory: ./testcases/dead_code_eliminate.txt 

III. Code Structure
--------------------------------------------------
Our code is implemented in "ir3toARM.ml". There are two major functions:

1) ir3_program_to_ARM: convert ir3 code to ARM code
2) ir3_program_to_ARM_optimized: generate optimized ARM code. It calls optimize_ir3 first to get optimized ir3 code, then calls ir3_program_to_ARM to generate ARM code.

IV. Design of Object Oriented Features
--------------------------------------------------
Please refer to "Design.txt" in this directory for details.
