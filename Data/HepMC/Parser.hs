parseEndEventsLine :: Parser ()
parseEndEventsLine = do
    string "HepMC::IO_GenEvent-END_EVENT_LISTING"
    skipSpace



parseXYZT :: Parser XYZT
parseXYZT = XYZT <$> doub <*> doub <*> doub <*> doub

