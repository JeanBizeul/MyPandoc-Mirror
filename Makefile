##
## EPITECH PROJECT, 2025
## MyPandoc-Mirror
## File description:
## Makefile
##

NAME		=	mypandoc

TESTS_NAME	=	unit_tests

all:	$(NAME)

$(NAME):
	stack build
	cp "$$(stack path --local-install-root)/bin/$(NAME)" .

clean:
	cabal clean
	stack clean

fclean:	clean
	rm -rf .stack-work 

re:	fclean $(NAME)

unit_tests:
	stack build --test --no-run-tests

tests_run:	unit_tests
	stack test

.PHONY:	all $(NAME) clean fclean re unit_tests tests_run
