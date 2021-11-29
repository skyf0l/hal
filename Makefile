NAME =	./hal

all:
	@stack build --copy-bins

clean:
	@stack clean

fclean: clean
	@stack purge
	@rm -f $(NAME)

re:
	@$(MAKE) -s fclean
	@$(MAKE) -s all

tests_run: all
	@./tests.sh