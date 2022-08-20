cmake_minimum_required(VERSION 3.20)

project(fdp2bin LANGUAGES CXX)

add_executable(fdp2bin
	"bigendian_io.h"
	"bitstream.h"
	"fdp2bin.cc"
	"kosinski.cc"
	"kosinski.h"
	"lzss.h"
)
