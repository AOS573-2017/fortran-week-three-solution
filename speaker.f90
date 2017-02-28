PROGRAM speaker
! PROGRAM speaker
! by Buckingham U. Badger
!
! This program prints something depending on the dialogue number provided.
!
! Created 02/10/17 BUB
IMPLICIT NONE

INTEGER :: dialoguenumber


!  |                                               |  !
! \|/ We need to ask for the dialogue number here \|/ !





! /|\ We need to ask for the dialogue number here /|\ !
!  |                                               |  !


! Here we print out the requested dialogue number.
IF (dialoguenumber .EQ. 1) THEN
    PRINT *, "Winter is coming."
ELSE IF (dialoguenumber .EQ. 2) THEN
    PRINT *, "These are not the droids you are looking for."
ELSE IF (dialoguenumber == 3) THEN
    PRINT *, "That _is_ a tasty burger."
ELSE IF (dialoguenumber == 4) THEN
    PRINT *, "I'm the king of the world."
ELSE IF ((dialoguenumber .EQ. 5) .OR. (dialoguenumber .EQ. 6)) THEN
    PRINT *, "I'm the greatest botanist on this planet."
ELSE IF ((dialoguenumber .EQ. 7) .OR. (dialoguenumber .EQ. 8) .OR. (dialoguenumber .EQ. 9)) THEN
    PRINT *, "So I guess I'm out of the book club."
ELSE IF ((dialoguenumber .EQ. 1) .AND. (dialoguenumber .EQ. 9)) THEN
    ! This will never happen because a single variable cannot equal two numbers
    PRINT *, "A wormhole's not a naturally occurring phenomenon."
ELSE
    ! In the event the user specify a number that is not a part of our dialogue
    PRINT *, "Sorry, but that dialogue--", dialoguenumber, "--does not exist."
END IF

END PROGRAM
