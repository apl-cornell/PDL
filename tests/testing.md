#Testing

The testing infrastructure uses the same command line interface as when running the main class.

##Adding a Test

- Add a test program to the corresponding folder: `<command>/examples`. 
- Add the expected output to the `<command>/solutions` folder with the file extension `".<command>sol"`. 
    - For example, if I made a parse solution, the extension would be `".parsesol"`.

##Running the Tests

- `cd` to the top level directory of the project.
- Build the compiler using `sbt assembly`
- Use `./run <command> --test --testResultDir <pathToResultDir> <pathToInputDir>` 
- The testing infrastructure will output the results of the command to a file `<pathToResultDir>/<testBaseName>.<command>` and compare the output file with the solution.
- The results are printed to console in the format `filename: Passed|Failed`
- ATM, if we expect parsing or typechecking to fail on a test, just put the word "Failed" in the solution file.

##Adding a test option:
- Add to the case statement in Main. 
- Add needed test functions to `src/main/scala/test/TestingMain`


  