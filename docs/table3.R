

## Testing the effect if we reduce the branch lenght form the actual value to 0.0 using evalTerminal {lower}

testEvalTerminalALL <- evalTerminal(RhinoclemmysData$tree,
                                    RhinoclemmysData$distribution,
                                    tipToEval = "all",
                                    approach = "lower")


print.multiBlepd(testEvalTerminalALL)


## Testing the effect if we increase the branch lenght form the actual value to the sum of all branch length  using evalTerminal {upper}


testEvalTerminalALL <- evalTerminal(RhinoclemmysData$tree,
                                    RhinoclemmysData$distribution,
                                    tipToEval = "all",
                                    approach = "upper")

print.multiBlepd(testEvalTerminalALL)

