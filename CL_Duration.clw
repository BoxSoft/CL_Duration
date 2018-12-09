                              MEMBER

  INCLUDE('CL_Duration.inc'),ONCE
! INCLUDE('Equates.clw'),ONCE

                              MAP
                              END

!==============================================================================
CL_Duration.Construct         PROCEDURE
  CODE
  SELF.TimeFormat = CL_Duration:TimeFormat:hh:mm:ss

  SELF.Abbr_Years   = 'y'
  SELF.Abbr_Months  = 'm'
  SELF.Abbr_Weeks   = 'w'
  SELF.Abbr_Days    = 'd'
  SELF.Abbr_Hours   = 'h'
  SELF.Abbr_Minutes = 'm'
  SELF.Abbr_Seconds = 's'

!==============================================================================
CL_Duration.Destruct            PROCEDURE
  CODE

!==============================================================================
CL_Duration.Format_           PROCEDURE(LONG Value,STRING Picture)!,STRING
  CODE
  RETURN CLIP(LEFT(FORMAT(Value, Picture)))
      
!==============================================================================
CL_Duration.FormatTime        PROCEDURE!,STRING
H                               LONG
M                               LONG
S                               LONG
  CODE
  CASE SELF.TimeFormat
    ;OF CL_Duration:TimeFormat:hh:mm:ss;  DO _hh:mm:ss
    ;OF CL_Duration:TimeFormat:hh:mm;     DO _hh:mm
    ;OF CL_Duration:TimeFormat:mm:ss;     DO _mm:ss
    ;OF CL_Duration:TimeFormat:h_m_s;     DO _h_m_s
    ;OF CL_Duration:TimeFormat:hms  ;     DO _hms
  END
  RETURN ''

_hh:mm:ss                     ROUTINE
  RETURN SELF.Format_(SELF.GetTime()+1, '@T4')

_hh:mm                        ROUTINE
  RETURN SELF.Format_(SELF.GetTime()+1, '@T4')  !TODO

_mm:ss                        ROUTINE
  RETURN SELF.Format_(SELF.GetTime()+1, '@T4')  !TODO

_h_m_s                        ROUTINE
  DO SetHMS
  RETURN    H & SELF.Abbr_Hours    |
      &' '& M & SELF.Abbr_Minutes  |
      &' '& S & SELF.Abbr_Seconds
  
_hms                          ROUTINE
  DO SetHMS
  RETURN H & SELF.Abbr_Hours    |
      &  M & SELF.Abbr_Minutes  |
      &  S & SELF.Abbr_Seconds
  
SetHMS                        ROUTINE
  DATA
V   REAL,AUTO
  CODE
  V = SELF.GetTime()
  H = INT(V / TIME:Hour) + 24*SELF.GetDays();  V -= H * TIME:Hour
  M = INT(V / TIME:Minute)                  ;  V -= M * TIME:Minute
  S = INT(V / TIME:Second)                  ;  V -= S * TIME:Second

!==============================================================================
CL_Duration.Get               PROCEDURE!,REAL
  CODE
  RETURN SELF._Value

!==============================================================================
CL_Duration.GetEither         PROCEDURE(<*LONG D>,<*LONG T>)
  CODE
  IF NOT OMITTED(D) THEN D = SELF.GetDays().
  IF NOT OMITTED(T) THEN T = SELF.GetTime().

!==============================================================================
CL_Duration.GetDays           PROCEDURE!,LONG
  CODE
  RETURN INT(SELF._Value / TIME:Day)
  
!==============================================================================
CL_Duration.GetTime           PROCEDURE!,LONG
  CODE
  RETURN SELF._Value % TIME:Day
  
!==============================================================================
CL_Duration.Set               PROCEDURE(REAL Value)
  CODE
  SELF._Value = Value
      
!==============================================================================
CL_Duration.SetEither         PROCEDURE(<LONG D>,<LONG T>)
  CODE
  SELF._Value =    CHOOSE(~OMITTED(T), T, SELF.GetTime())  |
      + TIME:Day * CHOOSE(~OMITTED(D), D, SELF.GetDays())

!==============================================================================
CL_Duration.SetDays           PROCEDURE(LONG D)
  CODE
  SELF.SetEither(D)

!==============================================================================
CL_Duration.SetTime           PROCEDURE(LONG T)
  CODE
  SELF.SetEither(, T)

!==============================================================================
CL_Duration.SetTimeFormat     PROCEDURE(BYTE TimeFormat)
  CODE
  SELF.TimeFormat = TimeFormat
  
!==============================================================================
