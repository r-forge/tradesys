library(tradesys)

testResults <- runTestSuite(allTradesysTests)
sink("tests.html")
printHTMLProtocol(testResults)
sink()
