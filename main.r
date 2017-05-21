#!/usr/local/bin/Rscript

LIVE <- 1
lockBinding('LIVE', globalenv())
DEAD <- 0
lockBinding('DEAD', globalenv())
UNDERPOPULATION <- 2 * LIVE
lockBinding('UNDERPOPULATION', globalenv())
OVERPOPULATION <- 3 * LIVE
lockBinding('OVERPOPULATION', globalenv())
REPRODUCTION <- 3 * LIVE
lockBinding('REPRODUCTION', globalenv())

conway.width <- 20
conway.height <- 10
conway.readFrom <- 1
conway.divider <- paste(rep('-', conway.width), collapse='')

conway.boardList <- list(matrix(DEAD, ncol=conway.width, nrow=conway.height, byrow=TRUE),
                         matrix(sample(DEAD:LIVE, conway.width*conway.height, replace=T), ncol=conway.width, nrow=conway.height, byrow=TRUE))
conway.print <- function(board) {
    if(is.matrix(board) == FALSE)
        stop('conway.print: argument one must be a matrix.')

    for(r in 1:nrow(board)) {
        row = board[r,]
        row[row==0] <- ' '
        row[row==1] <- 'X'
        print(paste(row, collapse=''))
    }
}

conway.swap <- function() {
    tmp <- conway.boardList[1]
    conway.boardList[1] <<- conway.boardList[2]
    conway.boardList[2] <<- tmp

    conway.boardList
}

conway.tick <- function() {
    conway.swap()

    readFromBoard = conway.boardList[[1]]

    for(row in 1:nrow(readFromBoard)) {
        for(col in 1:ncol(readFromBoard)) {
            s = 0
            cellState = readFromBoard[row, col]

            if( col-1 > 0 && row-1 > 0 )
                s = readFromBoard[row-1, col-1]
            if( row-1 > 0 )
                s = s + readFromBoard[row-1, col]
            if(col+1 <= ncol(readFromBoard) && row-1 > 0)
                s = s + readFromBoard[row-1, col+1]

            if(col-1 > 0)
                s = s + readFromBoard[row, col-1]
            if(col+1 <= ncol(readFromBoard))
                s = s + readFromBoard[row, col+1]
            
            if(col-1 > 0 && row+1 <= nrow(readFromBoard))
                s = s + readFromBoard[row+1, col-1]
            if(row+1 <= nrow(readFromBoard))
                s = s + readFromBoard[row+1, col]
            if(col+1 <= ncol(readFromBoard) && row+1 <= nrow(readFromBoard))
                s = s + readFromBoard[row+1, col+1]

            #print(paste('sum = ', s)) 

            if(cellState == LIVE){
                if(s < UNDERPOPULATION || s > OVERPOPULATION){
                    #print('KILL')
                    conway.boardList[[2]][row, col] <<- DEAD
                }else{
                    #Remain alive
                    #print('PASS')
                    conway.boardList[[2]][row, col] <<- LIVE
                }
            }else{
                if( s == REPRODUCTION ){
                    #print('BIRTH')
                    conway.boardList[[2]][row, col] <<- LIVE
                }else{
                    #remain dead
                    #print('DEAD')
                    conway.boardList[[2]][row, col] <<- DEAD
                }
            }
        }
    }
}

while(TRUE){
    conway.print(conway.boardList[[2]]);
    print(conway.divider)
    conway.tick()
    Sys.sleep(0.5)
}
