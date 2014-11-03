parseEndEventsLine :: Parser ()
parseEndEventsLine = do
    string "HepMC::IO_GenEvent-END_EVENT_LISTING"
    skipSpace



