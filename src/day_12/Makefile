CXX = clang++
CXXFLAGS = -Wall -std=c++17 -Iinclude -Ofast

CPP_FILES = $(wildcard *.cpp)
HEADERS_FILES = $(wildcard *.h)
OBJ_FILES = $(CPP_FILES:.cpp=.o)

EXEC_NAME = day_12

all: $(EXEC_NAME)

$(EXEC_NAME): $(OBJ_FILES)
	$(CXX) $(LDFLAGS) $(OBJ_FILES) -o $@

run: $(EXEC_NAME)
	./$(EXEC_NAME)

clean:
	rm -f $(OBJ_FILES)
	rm -f $(EXEC_NAME)
