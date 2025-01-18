q+

q+ | q+ToEnd |' >
q+ - q- - >
q+ + q+ + >
q+ = qF = -

q+ToEnd | q+ToEnd | >
q+ToEnd - q+ToEnd - >
q+ToEnd + q+ToEnd + >
q+ToEnd = q+ToEnd = >
q+ToEnd _ qMovePlusLeftPastResult | <

q- | q-ToEnd |' >
q- - q- - >
q- + q+ + >
q- = qF = -

q-ToEnd | q-ToEnd | >
q-ToEnd + q-ToEnd + >
q-ToEnd = q-ToEnd = >
q-ToEnd _ qDel _ <

qDel | qMoveMinusLeftPastResult _ <

qMovePlusLeftPastResult | qMovePlusLeftPastResult | <
qMovePlusLeftPastResult = qMovePlusLeftToMarked = <

qMoveMinusLeftPastResult | qMoveMinusLeftPastResult | <
qMoveMinusLeftPastResult = qMoveMinusLeftToMarked = <

qMovePlusLeftToMarked | qMovePlusLeftToMarked | <
qMovePlusLeftToMarked - qMovePlusLeftToMarked - <
qMovePlusLeftToMarked + qMovePlusLeftToMarked + <
qMovePlusLeftToMarked |' q+ |' >

qMoveMinusLeftToMarked | qMoveMinusLeftToMarked | <
qMoveMinusLeftToMarked + qMoveMinusLeftToMarked + <
qMoveMinusLeftToMarked - qMoveMinusLeftToMarked - <
qMoveMinusLeftToMarked |' q- |' >
