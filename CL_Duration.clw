                              MEMBER

  INCLUDE('CL_Duration.inc'),ONCE
! INCLUDE('Equates.clw'),ONCE

                              MAP
                              END

!==============================================================================
CL_Duration.Construct         PROCEDURE
  CODE
  SELF.SetFormat(CL_Duration:Format:hh:mm:ss)

!==============================================================================
CL_Duration.Destruct          PROCEDURE
  CODE

!==============================================================================
CL_Duration.SetFormat         PROCEDURE(BYTE DurationFormat)
  CODE
  SELF._Format = DurationFormat
  
!==============================================================================
CL_Duration.FormatDuration    PROCEDURE!,STRING
H                               LONG
M                               LONG
S                               LONG
  CODE
  CASE SELF._Format
    ;OF CL_Duration:Format:hh:mm:ss;  DO _hh:mm:ss
    ;OF CL_Duration:Format:h_m_s;     DO _h_m_s
    ;OF CL_Duration:Format:hms  ;     DO _hms
  END
  RETURN ''

_hh:mm:ss                     ROUTINE
  RETURN SELF.TrimFormat(SELF.GetTime()+1, '@T4')

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
CL_Duration.TrimFormat        PROCEDURE(LONG Value,STRING Picture)!,STRING
  CODE
  RETURN CLIP(LEFT(FORMAT(Value, Picture)))
      
!==============================================================================
