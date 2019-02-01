                              MEMBER

  INCLUDE('CL_Duration.inc'),ONCE
! INCLUDE('Equates.clw'),ONCE

                              MAP
                                INCLUDE('STDebug.inc')
                              END

!==============================================================================
CL_Duration.Construct         PROCEDURE
  CODE
  SELF.SetUnits(CL_Duration:Unit:Hours, CL_Duration:Unit:Seconds)
  SELF.SetDelimiter(':')
  SELF.SetUseAbbreviations(FALSE)
  SELF.SetMostSignificantUnitPadZero(FALSE)
  SELF.SetInsignificantUnitsPadZero(TRUE)
  SELF.SetOmitZeros(FALSE)
  
  !SELF.Abbr_Years   = 'y'
  !SELF.Abbr_Months  = 'm'
  SELF.Abbr_Weeks   = 'w'
  SELF.Abbr_Days    = 'd'
  SELF.Abbr_Hours   = 'h'
  SELF.Abbr_Minutes = 'm'
  SELF.Abbr_Seconds = 's'

!==============================================================================
CL_Duration.Destruct          PROCEDURE
  CODE

!==============================================================================
CL_Duration.SetUnits          PROCEDURE(BYTE MostSignificantUnit,<BYTE LeastSignificantUnit>,<BYTE DefaultUnit>)
  CODE
  SELF.MostSignificantUnit  = MostSignificantUnit
  SELF.LeastSignificantUnit = CHOOSE(~OMITTED(LeastSignificantUnit), LeastSignificantUnit, MostSignificantUnit)
  SELF.DefaultUnit          = CHOOSE(~OMITTED(DefaultUnit         ), DefaultUnit         , MostSignificantUnit)

!==============================================================================
!CL_Duration.SetFormat         PROCEDURE(LONG GeneralType)
!  CODE
  
!==============================================================================
CL_Duration.SetDelimiter      PROCEDURE(STRING Delimiter)
  CODE
  SELF.Delimiter = Delimiter

!==============================================================================
CL_Duration.SetUseAbbreviations   PROCEDURE(BOOL UseAbbreviations)
  CODE
  SELF.UseAbbreviations = UseAbbreviations

!==============================================================================
CL_Duration.SetMostSignificantUnitPadZero PROCEDURE(BOOL MostSignificantUnitPadZero)
  CODE
  SELF.MostSignificantUnitPadZero = MostSignificantUnitPadZero

!==============================================================================
CL_Duration.SetInsignificantUnitsPadZero  PROCEDURE(BOOL InsignificantUnitsPadZero)
  CODE
  SELF.InsignificantUnitsPadZero = InsignificantUnitsPadZero

!==============================================================================
CL_Duration.SetOmitZeros      PROCEDURE(BOOL OmitZeros)
  CODE
  SELF.OmitZeros = OmitZeros

!==============================================================================
CL_Duration.FormatDuration    PROCEDURE!,STRING
                              MAP
SetUnitValues                   PROCEDURE
AppendUnitValues                PROCEDURE
                              END
Unit                            BYTE
UnitValue                       LONG,DIM(CL_Duration:Unit:_LAST)
H                               LONG
M                               LONG
S                               LONG
ReturnValue                     CSTRING(100)
  CODE
  SetUnitValues()
  AppendUnitValues()
  RETURN ReturnValue

!--------------------------------------
SetUnitValues                 PROCEDURE
RemainingValue                  REAL,AUTO
UnitDuration                    LONG
  CODE
  RemainingValue = TIME:Day * SELF.GetDays() + SELF.GetTime()
  LOOP Unit = SELF.MostSignificantUnit TO SELF.LeastSignificantUnit BY -1
    EXECUTE Unit
      UnitDuration = TIME:Second
      UnitDuration = TIME:Minute
      UnitDuration = TIME:Hour
      UnitDuration = TIME:Day
      UnitDuration = TIME:Day * 7
    ELSE
      BEGIN
        !STOP('Unexpected Unit value in CL_Duration/FormatDuration: '& Unit)
        ASSERT(FALSE, 'Unexpected Unit value in CL_Duration/FormatDuration: '& Unit)
      END
    END
    UnitValue[Unit] = INT(RemainingValue / UnitDuration)
    RemainingValue -= UnitValue[Unit] * UnitDuration
  END

!--------------------------------------
AppendUnitValues              PROCEDURE
                              MAP
CalcZeroPad                     PROCEDURE(BOOL PaddingZero),STRING
                              END
SpaceBeforeAbbrev               CSTRING(2)
UnitAbbrev                      CSTRING(11)
PluralAbbrev                    CSTRING(2)
Delimiter                       CSTRING(10)
  CODE
  SpaceBeforeAbbrev = CHOOSE(~SELF.SpaceBeforeAbbreviation, '', ' ')
  LOOP Unit = SELF.MostSignificantUnit TO SELF.LeastSignificantUnit BY -1
    IF SELF.OmitZeros AND UnitValue[Unit] = 0 THEN CYCLE.
    IF SELF.UseAbbreviations
      PluralAbbrev = CHOOSE(SELF.PluralizeAbbreviation AND UnitValue[Unit] <> 1, 's', '')
      EXECUTE Unit
        UnitAbbrev = SpaceBeforeAbbrev & SELF.Abbr_Seconds & PluralAbbrev
        UnitAbbrev = SpaceBeforeAbbrev & SELF.Abbr_Minutes & PluralAbbrev
        UnitAbbrev = SpaceBeforeAbbrev & SELF.Abbr_Hours   & PluralAbbrev
        UnitAbbrev = SpaceBeforeAbbrev & SELF.Abbr_Days    & PluralAbbrev
        UnitAbbrev = SpaceBeforeAbbrev & SELF.Abbr_Weeks   & PluralAbbrev
      ELSE
        BEGIN
          !STOP('Unexpected Unit value in CL_Duration/FormatDuration: '& Unit)
          ASSERT(FALSE, 'Unexpected Unit value in CL_Duration/FormatDuration: '& Unit)
        END
      END
    END
    ReturnValue = ReturnValue |
        & Delimiter |
        & CalcZeroPad(CHOOSE(ReturnValue='', SELF.MostSignificantUnitPadZero, SELF.InsignificantUnitsPadZero)) |
        & UnitValue[Unit] |
        & UnitAbbrev
    Delimiter = SELF.Delimiter
  END

CalcZeroPad                   PROCEDURE(BOOL PaddingZero)!,STRING
  CODE
  RETURN CHOOSE(UnitValue[Unit] < 10 AND PaddingZero, '0', '')

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
CL_Duration.Parse             PROCEDURE(STRING Value)!,LONG
Q                               QUEUE
Number                            REAL
Text                              STRING(10)
                                END
ReturnValue                     LONG(0)
  CODE
  DO CheckForNumberWithoutText  !Returns from Routine
  DO ParseValue
  DO AccumulatePortions
  RETURN ReturnValue

CheckForNumberWithoutText     ROUTINE
  IF NUMERIC(Value)
    RETURN Value * SELF.UnitToDuration(SELF.DefaultUnit)
  END

ParseValue                    ROUTINE
  DATA
L   LONG
P   BYTE
    ITEMIZE
State:Begin   EQUATE
State:Number  EQUATE
State:Text    EQUATE
    END
State   BYTE(State:Begin)
  CODE
  L = LEN(CLIP(Value))
  ASSERT(L < 256, 'Value parameter too long in CL_Duration.Parse')
  
  LOOP P = 1 TO L
    CASE State
    OF State:Begin
      CLEAR(Q)
      CASE Value[P]
      OF '0' TO '9' OROF '.'
        State = State:Number
        Q.Number = Value[P]
      END
    OF State:Number
      CASE Value[P]
      OF '0' TO '9' OROF '.'
        Q.Number = CLIP(Q.Number) & Value[P]
      ELSE
        State = State:Text
        Q.Text = Value[P]
      END
    OF State:Text
      CASE Value[P]
      OF '0' TO '9' OROF '.'
        ADD(Q)
        CLEAR(Q)
        State = State:Number
        Q.Number = Value[P]
      ELSE
        State = State:Text
        Q.Text = CLIP(Q.Text) & Value[P]
      END
    END
  END
  IF Q.Number <> ''
    ADD(Q)
  END
  !ST::DebugQueue(Q)

AccumulatePortions            ROUTINE
  DATA
X   BYTE
  CODE
  LOOP X = 1 TO RECORDS(Q)
    GET(Q, X)
    CASE LEFT(Q.Text)
      !;OF LEFT(SELF.Abbr_Years)
      !;OF LEFT(Abbr_Months)
      ;OF LEFT(SELF.Abbr_Weeks  );  ReturnValue += Q.Number * TIME:Day * 7
      ;OF LEFT(SELF.Abbr_Days   );  ReturnValue += Q.Number * TIME:Day
      ;OF LEFT(SELF.Abbr_Hours  );  ReturnValue += Q.Number * TIME:Hour
      ;OF LEFT(SELF.Abbr_Minutes);  ReturnValue += Q.Number * TIME:Minute
      ;OF LEFT(SELF.Abbr_Seconds);  ReturnValue += Q.Number * TIME:Second
    END
  END
  
!==============================================================================
CL_Duration.UnitToDuration    PROCEDURE(BYTE Unit)!,LONG
  CODE
  CASE SELF.DefaultUnit
    ;OF CL_Duration:Unit:Seconds;  RETURN TIME:Second
    ;OF CL_Duration:Unit:Minutes;  RETURN TIME:Minute
    ;OF CL_Duration:Unit:Hours  ;  RETURN TIME:Hour
    ;OF CL_Duration:Unit:Days   ;  RETURN TIME:Day
    ;OF CL_Duration:Unit:Weeks  ;  RETURN TIME:Day * 7
!    ;OF CL_Duration:Unit:Months ;  RETURN TIME:Second
!    ;OF CL_Duration:Unit:Years  ;  RETURN TIME:Second
  END
  ASSERT(FALSE, 'Invalid Unit='& Unit &' parameter in CL_Duration.UnitToDuration')
  RETURN -1

!==============================================================================
