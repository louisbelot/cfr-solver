(** Leduc hold'em (Southey et al., 2005), with OpenSpiel's rules.

    A six-card deck holds two jacks, two queens and two kings. Both players ante
    1 and get one private card, then bet; a public board card is dealt and they
    bet again. Raises are 2 in the first round and 4 in the second, with at most
    two raises per round, and player 0 acts first in both. A player who pairs
    the board wins the showdown, otherwise the higher private card does, and
    equal cards split the pot.

    Suits never matter, so the chance nodes deal ranks with probability
    proportional to how many are left. That merges histories that differ only by
    suit and gives 288 infosets instead of OpenSpiel's 936, for the same game.

    Actions are ["f"] (fold, offered only when facing a raise), ["c"] (check or
    call) and ["r"] (raise, while fewer than two raises have been made this
    round), in that order. Infoset keys are the player's card, the board card
    once dealt, a colon, then the betting with a slash between rounds: ["K:cr"],
    ["QK:rc/"], ["JJ:cc/rr"]. *)

include Game.S
