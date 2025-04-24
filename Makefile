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
	stack build --allow-different-user
	cp "$$(stack path --local-install-root)/bin/$(NAME)" .

clean:
	stack clean --allow-different-user

fclean:	clean
	rm -rf .stack-work 

re:	fclean $(NAME)

unit_tests:
	stack build --test --no-run-tests --allow-different-user

tests_run:	unit_tests
	stack test --allow-different-user

.PHONY:	all $(NAME) clean fclean re unit_tests tests_run
