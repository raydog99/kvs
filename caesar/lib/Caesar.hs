import CapnpProto

data AcceptorState = AcceptorState {
  ballot :: Ballot,
  }

data LeaderState = LeaderState {
  ballot :: Ballot,
  whitelist :: [AcceptorId],
  }

fastProposeLeader :: LeaderState -> Command -> CapnpChannel -> IO FastProposeResult
fastProposeLeader leaderState cmd channel = do
  let fastProposeMsg = FastPropose {
        command = cmd,
        ballot = ballot leaderState,
        time = getCurrentTime,
        whitelist = whitelist leaderState
      }
  sendMsg channel "fastPropose" fastProposeMsg
  responses <- waitForAllResponses channel (map AcceptorId [1..])
  let (okCount, nackCount) = countResponses responses
  if nackCount > 0 then
    return $ FastProposeResult NACK
  else do
    let predecessors = computePredecessors responses
    newState <- updateState leaderState predecessors cmd
    sendOutcomeToAcceptors channel newState okCount
    return $ FastProposeResult (Just newState)

fastProposeAcceptor :: AcceptorState -> FastPropose -> CapnpChannel -> IO ()
fastProposeAcceptor acceptorState msg@(FastPropose {ballot = leaderBallot}) channel = do
  let currentBallot = ballot acceptorState
  if leaderBallot > currentBallot then do
    let updatedState = updateState acceptorState msg
    sendResponse channel "fastProposer" (FastProposer OK updatedState)
  else
    sendResponse channel "fastProposer" (FastProposer NACK acceptorState)

waitForAllResponses :: CapnpChannel -> [AcceptorId] -> IO [Maybe FastProposer]
waitForAllResponses channel acceptorIds = do
  responses <- mapM (getResponse channel) acceptorIds
  return responses

countResponses :: [Maybe FastProposer] -> (Int, Int)
countResponses responses = foldr (\acc maybeResp -> case maybeResp of
                                 Just (FastProposer OK) -> (fst acc + 1, snd acc)
                                 Just (FastProposer NACK) -> (fst acc, snd acc + 1)
                                 Nothing -> acc) (0, 0) responses

sendOutcomeToAcceptors :: CapnpChannel -> LeaderState -> Int -> IO ()
sendOutcomeToAcceptors channel state okCount = do
  let outcome = if okCount > threshold then Just state else NACK
  fastProposeMsg <- toMessage outcome
  sendMsg channel "fastProposer" fastProposeMsg