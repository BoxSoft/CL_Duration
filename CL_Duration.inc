!ABCIncludeFile

  INCLUDE('DateTime.equ'),ONCE
         
                              ITEMIZE
CL_Duration:Unit:Seconds        EQUATE
CL_Duration:Unit:Minutes        EQUATE
CL_Duration:Unit:Hours          EQUATE
CL_Duration:Unit:Days           EQUATE
CL_Duration:Unit:Weeks          EQUATE
CL_Duration:Unit:Months         EQUATE
CL_Duration:Unit:Years          EQUATE
                              END                                

                              ITEMIZE
CL_Duration:Format:nn:nn:nn     EQUATE
CL_Duration:Format:nn:nn        EQUATE
CL_Duration:TimeFormat:h_m_s    EQUATE
CL_Duration:TimeFormat:hms      EQUATE
                              END

CL_Duration                   CLASS,TYPE,MODULE('CL_Duration.clw'),LINK('CL_Duration.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
_Value                          REAL,PROTECTED
LargestUnit                     BYTE,PROTECTED
SmallestUnit                    BYTE,PROTECTED
FormatSeparator                 CSTRING(3),PROTECTED
FormatWithAbbr                  BOOL,PROTECTED
TimeFormat                      BYTE,PROTECTED

Abbr_Years                      CSTRING(10)
Abbr_Months                     CSTRING(10)
Abbr_Weeks                      CSTRING(10)
Abbr_Days                       CSTRING(10)
Abbr_Hours                      CSTRING(10)
Abbr_Minutes                    CSTRING(10)
Abbr_Seconds                    CSTRING(10)

Construct                       PROCEDURE
Destruct                        PROCEDURE,VIRTUAL

SetUnits                        PROCEDURE(BYTE LargestUnit,BYTE SmallestUnit)
SetFormat                       PROCEDURE(BYTE FormatEquate)
SetFormat                       PROCEDURE(STRING Separator,BOOL WithAbbr)

Format_                         PROCEDURE(LONG Value,STRING Picture),STRING
FormatTime                      PROCEDURE,STRING
Get                             PROCEDURE,REAL
GetEither                       PROCEDURE(<*LONG D>,<*LONG T>)
GetDays                         PROCEDURE,LONG
GetTime                         PROCEDURE,LONG
Set                             PROCEDURE(REAL Value)
SetEither                       PROCEDURE(<LONG D>,<LONG T>)
SetDays                         PROCEDURE(LONG D)
SetTime                         PROCEDURE(LONG T)
SetTimeFormat                   PROCEDURE(BYTE TimeFormat)
                              END
