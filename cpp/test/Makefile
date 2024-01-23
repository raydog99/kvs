CXX = g++
CXXFLAGS = -std=c++11
LDFLAGS = -pthread
SRC = test_base_protocol.cpp
OBJ = $(SRC:.cpp=.o)
TARGET = test_base_protocol

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(OBJ) $(TARGET)
