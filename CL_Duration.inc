!ABCIncludeFile

  INCLUDE('DateTime.equ'),ONCE
         
                              ITEMIZE
CL_Duration:Unit:_FIRST         EQUATE(1)
CL_Duration:Unit:Seconds        EQUATE(CL_Duration:Unit:_FIRST)
CL_Duration:Unit:Minutes        EQUATE
CL_Duration:Unit:Hours          EQUATE
CL_Duration:Unit:Days           EQUATE
CL_Duration:Unit:Weeks          EQUATE
!CL_Duration:Unit:Months         EQUATE
!CL_Duration:Unit:Years          EQUATE
CL_Duration:Unit:_LAST          EQUATE(CL_Duration:Unit:Weeks)
                              END                                

!                              ITEMIZE
!CL_Duration:Format:hh:mm:ss     EQUATE   !hh:mm:ss
!CL_Duration:Format:h_m_s        EQUATE   !1h 2m 3s
!CL_Duration:Format:hms          EQUATE   !1h2m3s
!                              END

CL_Duration                   CLASS,TYPE,MODULE('CL_Duration.clw'),LINK('CL_Duration.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
_Value                          REAL,PROTECTED

MostSignificantUnit             BYTE,PROTECTED
LeastSignificantUnit            BYTE,PROTECTED
DefaultUnit                     BYTE,PROTECTED

!Abbr_Years                      CSTRING(10)
!Abbr_Months                     CSTRING(10)
Abbr_Weeks                      CSTRING(10)
Abbr_Days                       CSTRING(10)
Abbr_Hours                      CSTRING(10)
Abbr_Minutes                    CSTRING(10)
Abbr_Seconds                    CSTRING(10)

Delimiter                       CSTRING(10)
UseAbbreviations                BOOL
SpaceBeforeAbbreviation         BOOL
PluralizeAbbreviation           BOOL
MostSignificantUnitPadZero      BOOL
InsignificantUnitsPadZero       BOOL
OmitZeros                       BOOL

Construct                       PROCEDURE
Destruct                        PROCEDURE,VIRTUAL

SetUnits                        PROCEDURE(BYTE MostSignificantUnit,<BYTE LeastSignificantUnit>,<BYTE DefaultUnit>)
SetDelimiter                    PROCEDURE(STRING Delimiter)
SetUseAbbreviations             PROCEDURE(BOOL UseAbbreviations)
SetMostSignificantUnitPadZero   PROCEDURE(BOOL MostSignificantUnitPadZero)
SetInsignificantUnitsPadZero    PROCEDURE(BOOL InsignificantUnitsPadZero)
SetOmitZeros                    PROCEDURE(BOOL OmitZeros)

FormatDuration                  PROCEDURE,STRING
Parse                           PROCEDURE(STRING Value),LONG
Get                             PROCEDURE,REAL
GetEither                       PROCEDURE(<*LONG D>,<*LONG T>)
GetDays                         PROCEDURE,LONG
GetTime                         PROCEDURE,LONG
Set                             PROCEDURE(REAL Value)
SetEither                       PROCEDURE(<LONG D>,<LONG T>)
SetDays                         PROCEDURE(LONG D)
SetTime                         PROCEDURE(LONG T)

TrimFormat                      PROCEDURE(LONG Value,STRING Picture),STRING,PROTECTED
UnitToDuration                  PROCEDURE(BYTE Unit),LONG
                              END
