
col_letter_to_index <- function(col_char) {

  out = NA

  if (col_char == "A") {
    out = 1
  } else if (col_char == "B") {
    out = 2
  } else {
    out = 3
  }

  out

}

check_board <- function(board) {

  out = NA
  dg1 = diag(board)
  dg2 = diag(board[,c(3:1),drop=FALSE])

  if (any(all(board[1,]==1),
          all(board[2,]==1),
          all(board[3,]==1),
          all(board[,1]==1),
          all(board[,2]==1),
          all(board[,3]==1)) ||
      all(dg1 == 1) ||
      all(dg2 == 1)){
    out = 1
  }

  if (any(all(board[1,]==2),
          all(board[2,]==2),
          all(board[3,]==2),
          all(board[,1]==2),
          all(board[,2]==2),
          all(board[,3]==2)) ||
      all(dg1 == 2) ||
      all(dg2 == 2)) {

    out = 2

  }

  out

}

print_board <- function(board) {

  symbols = c(" ", "O", "X")

  row0 = "   A B C\n"
  row1 = paste("1 |", symbols[board[1,1]+1],
               "|", symbols[board[1,2]+1],
               "|", symbols[board[1,3]+1],
               "|\n", sep="")
  row2 = paste("2 |", symbols[board[2,1]+1],
               "|", symbols[board[2,2]+1],
               "|", symbols[board[2,3]+1],
               "|\n", sep="")
  row3 = paste("3 |", symbols[board[3,1]+1],
               "|", symbols[board[3,2]+1],
               "|", symbols[board[3,3]+1],
               "|\n", sep="")

  cat(paste(row0, row1, row2, row3, sep=""))
}
