#Testing

Automated tests use sbt's built in testing infrastructure

##Adding a Test/Tests

- Create a folder for the tests in `src/test/tests/<folderName>`
- Put the relevant tests in the created folder
- Create a folder `src/test/tests/<folderName>/solutions` and put expected output files for the tests
    - Solution files should be of the form `<testName>.<command>sol`
    - For example, an expected output parse file would be `test.parsesol"
- Create a folder `src/test/tests/<folderName>/memInputs` and put any memory initializations for simulation in the folder
- Add a test in `src/test/scala/pipedsl/MainSuite`, following the current tests as examples

##Running the Tests

- run `sbt test` from the top level directory 


  