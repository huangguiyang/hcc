INSTALL_ROOT=/usr/local
INSTALL_BIN=$INSTALL_ROOT/bin
INSTALL_LIB=$INSTALL_ROOT/lib
TARGET=hcc
CFLAGS+=-Wall -O2 -fno-strict-aliasing -Wno-missing-braces -Wno-char-subscripts
LDFLAGS=

# obects
OBJS = main.o \
	liberty.o \
	symtab.o \
	tokens.o \
	types.o \
	fmt.o \
	cpp.o \
	cc.o \
	parse.o \
	sub.o \
	typechk.o \
	gen.o \
	section.o \
	x86_64-gen.o \
	x86_64-opc.o

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@

headers::
	mkdir -p $(INSTALL_BIN)/$(TARGET)
	cp -R include $(INSTALL_LIB)/$(TARGET)

install:: headers $(TARGET)
	mkdir -p $(INSTALL_BIN)
	cp $(TARGET) $(INSTALL_BIN)

uninstall::
	rm -f $(INSTALL_BIN)/$(TARGET)
	rm -rf $(INSTALL_LIB)/$(TARGET)

clean::
	@find . \( -name '.git' \) -prune -o \
		\( -name '*.o' -o -name '*~' -o -name '*.s' \) \
		-type f -print | xargs rm -f
	@rm -f $(TARGET) stage1 stage2 stage3

# bootstrap

objclean::
	@rm -f *.o

stage1:
	$(MAKE) objclean
	$(MAKE) CC=cc
	mv $(TARGET) stage1

stage2: stage1
	$(MAKE) objclean
	$(MAKE) CC=./stage1
	mv $(TARGET) stage2

stage3: stage2
	$(MAKE) objclean
	$(MAKE) CC=./stage2
	mv $(TARGET) stage3

bootstrap:: stage3
	cmp stage2 stage3

# Test
test::
	@cd test && make

help::
	@echo "Usage:"
	@echo " make            # Compile the compiler"
	@echo " make install    # Compile and install the compiler"
	@echo " make clean      # Remove all compiler results and editor backups"
	@echo " make uninstall  # Uninstall the compiler"

# depend
cc.o: cc.c cc.h liberty.h
cpp.o: cpp.c compat.h cc.h liberty.h
fmt.o: fmt.c cc.h liberty.h
gen.o: gen.c cc.h liberty.h
liberty.o: liberty.c compat.h liberty.h
main.o: main.c config.h compat.h cc.h liberty.h
parse.o: parse.c cc.h liberty.h
section.o: section.c cc.h liberty.h elf.h
sub.o: sub.c cc.h liberty.h
symtab.o: symtab.c cc.h liberty.h
tokens.o: tokens.c cc.h liberty.h
typechk.o: typechk.c cc.h liberty.h
types.o: types.c cc.h liberty.h
x86_64-gen.o: x86_64-gen.c config.h compat.h cc.h liberty.h x86_64.h elf.h
x86_64-opc.o: x86_64-opc.c cc.h liberty.h x86_64.h
# end depend
