allTradesysTests <- defineTestSuite("All Tests", 
   dirs=system.file("tests", package="tradesys"), 
   testFileRegexp="Test.R$")

runAllTradesysTests <- function() {
   testResults <- runTestSuite(allTradesysTests)
   printTextProtocol(testResults)
}
