!ABCIncludeFile

  INCLUDE('DateTime.equ'),ONCE
         
                              ITEMIZE
CL_Duration:Format:hh:mm:ss     EQUATE
CL_Duration:Format:h_m_s        EQUATE
CL_Duration:Format:hms          EQUATE
                              END

CL_Duration                   CLASS,TYPE,MODULE('CL_Duration.clw'),LINK('CL_Duration.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
_Value                          REAL,PROTECTED
_Format                         BYTE,PROTECTED

Abbr_Years                      CSTRING(10)
Abbr_Months                     CSTRING(10)
Abbr_Weeks                      CSTRING(10)
Abbr_Days                       CSTRING(10)
Abbr_Hours                      CSTRING(10)
Abbr_Minutes                    CSTRING(10)
Abbr_Seconds                    CSTRING(10)

Construct                       PROCEDURE
Destruct                        PROCEDURE,VIRTUAL

SetFormat                       PROCEDURE(BYTE DurationFormat)

FormatDuration                  PROCEDURE,STRING
Get                             PROCEDURE,REAL
GetEither                       PROCEDURE(<*LONG D>,<*LONG T>)
GetDays                         PROCEDURE,LONG
GetTime                         PROCEDURE,LONG
Set                             PROCEDURE(REAL Value)
SetEither                       PROCEDURE(<LONG D>,<LONG T>)
SetDays                         PROCEDURE(LONG D)
SetTime                         PROCEDURE(LONG T)

TrimFormat                      PROCEDURE(LONG Value,STRING Picture),STRING,PROTECTED
                              END
