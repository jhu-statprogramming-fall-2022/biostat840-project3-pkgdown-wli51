#' Start a tic-tac-toe game on R console
#'
#' The player gets to take the first turn, the computer randomly fills in one
#' of the empty spots each turn.
#'
#' @details Start a tic-tac-toe game on R console. The user can specify the
#' position to mark with command line input. The player takes the first turn,
#' and the computer marks one spot randomly after every player turn. The board
#' is printed to the console.
#'
#' @param demo logical, if set to be true, player input will not be needed and
#' every mark by the player will be randomly decided
#'
#' @return nothing useful is returned.
#'
#' @import stringr
#' @export
#'
#' @examples tictactoe(demo=TRUE)
#'
tictactoe <- function(demo=FALSE) {

  board = matrix(data=rep(0,9), nrow=3, ncol=3)
  player_turn = TRUE


  while(TRUE) {


    if (sum(board==0) <= 0 || !is.na(check_board(board))) {
      break
    }

    if (player_turn) {

      if (demo) {

        auto_play = sample(which(board==0),1)
        board[auto_play] = 1
        player_turn = FALSE
        print_board(board)

      } else {

        print_board(board)
        cat("Awaiting input...\n")
        input <- readline();

        if (input == "q" || input == "Q") {
          cat("Please confirm Quit [Y/n]\n")
          input <- readline();
          if (input == "y" || input == "Y") {
            break
          }
        } else {
          match = str_extract(toupper(input), "[ABC][123]")
          if (!is.na(match)) {

            col = col_letter_to_index(substr(match,1,1))
            row = as.integer(substr(match,2,2))

            if (board[row, col] == 0) {

              board[row, col] = 1
              player_turn = FALSE

            } else {

              cat("Position already occupied!\n")

            }

          } else {

            cat("Invalid input\n")

          }
        }

      }


    }
    else {# computer turn

      player_turn = TRUE

      cpu_turn = sample(which(board==0),1)
      board[cpu_turn] = 2
    }
  }

  print_board(board)
  if(check_board(board) == 1) {
    cat("You win!\n")
  } else if (check_board(board) == 2) {
    cat("You lose!\n")
  } else {
    cat("Draw")
  }

}
