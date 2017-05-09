# mOOL to ARM Assembly

This is the ARM assembly code generation assignment in NUS compiler course. A Java-like language with a limited set of object oriented features called mOOL is defined in this course. The intermediate representation of mOOL is translated into ARM assembly in this assignment.

## Design of Object Oriented Features

Due to time constraints, the object-oriented features are not implemented. The high level design details for the object-oriented features are specified in `Design.txt`.

## Optimizations

The following optimizations are applied in this assignment:

1. Liveness analysis is applied to optimize register usage.
2. Machine independent optimizations including common sub-expression replacement, copy propogation and dead code elimination are applied to intermediate code.

## Compilation and Running

#### 1. Generate compiler

Run `make` at this directory. The compiler is generated as `mOOL_compiler`.

##### 2. Compile MOOL code to ARM code

a) Non-optimized mode:

Run `./mOOL_compiler <directory of test program>`. The generated ARM code will be written as `arm.s` in this directory.

b) Optimized mode:

Run `./mOOL_compiler o <directory of test program>`. The generated ARM code will be written as `arm.s` in this directory.

#### 3. Run ARM code in emulator shell

a) Start Android emulator.  
b) Use `arm-linux-androideabi-g++` to compile `arm.s` into `out`.  
c) Copy `out` into `/data/local/tmp/` directory of emulator using `adb`.  
d) In emulator shell, run `/data/local/tmp/out`.

## Test Cases

1. Hello world test: prints "Hello world" and prints results of some simple expression.  
Directory: `./testcases/hello_world.txt`

2. Sum 1 to 100 test: prints results of sum from 1 to 100.  
Directory: `./testcases/sum_one_to_hundred.txt`

3. Factorial test: prints factorial results.  
Directory: `./testcases/factorial.txt`

4. Print shape test: prints shape of rectangle by using "*".  
Directory: `./testcases/print_shape.txt`

5. Multiple classes & method more than 4 parameters test:  
Directory: `./testcases/multiple_classes_and_multiple_params.txt`

6. Code optimization test:  
Directory: `./testcases/code_optimization.txt`

7. Dead code elimination test:  
Directory: `./testcases/dead_code_eliminate.txt`

## Code Structure

The code is implemented in `ir3toARM.ml`. There are two major functions:

1. `ir3_program_to_ARM`: convert ir3 code to ARM code

2. `ir3_program_to_ARM_optimized`: generate optimized ARM code. It calls `optimize_ir3` first to get optimized ir3 code, then calls `ir3_program_to_ARM` to generate ARM code.

