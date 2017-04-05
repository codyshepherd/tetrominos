
SRC = src
S = scala
SC = scalac
TARGET = out
CP = /u/cls9/CS542/ai_combinatorial/out

default:
	@echo "Compiling..."
	@mkdir out
	@$(SC) -d $(TARGET) $(SRC)/*.scala

clean:
	@rm -r $(TARGET)
