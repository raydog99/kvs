package kvs

import "C"
import (
	"errors"
	"unsafe"
)

// ECBackend represents the liberasurecode backend type.
type ECBackend int

const (
	EC_BACKEND_LIBERASURECODE_RS_VAND ECBackend = iota
	EC_BACKEND_JERASURE_RS_VAND
)

// ECArgs represents the arguments required for liberasurecode operations.
type ECArgs struct {
	k  C.uint32_t
	m  C.uint32_t
	w  C.uint32_t
	ct C.int
}

// CreateLiberasureInstance creates a liberasurecode instance with given parameters.
func CreateLiberasureInstance(m, k uint32) (int, error) {
	var args C.struct_ec_args
	args.k = C.uint32_t(k)
	args.m = C.uint32_t(m - k)
	args.w = 16 // Adjust as necessary
	args.ct = C.CHKSUM_NONE

	desc := int(C.liberasurecode_instance_create(C.int(EC_BACKEND_LIBERASURECODE_RS_VAND), &args))
	if desc < 0 {
		return desc, errors.New("liberasurecode instance creation failed")
	}
	return desc, nil
}

// DestroyLiberasureInstance destroys the liberasurecode instance.
func DestroyLiberasureInstance(desc int) {
	C.liberasurecode_instance_destroy(C.int(desc))
}

// Encode performs data encoding using liberasurecode.
func Encode(data []byte, args *ECArgs, desc int) ([][]byte, error) {
	var encodedData **C.char
	var encodedParity **C.char
	var encodedFragmentLen C.uint64_t

	rc := C.liberasurecode_encode(C.int(desc), (*C.char)(unsafe.Pointer(&data[0])), C.size_t(len(data)),
		&encodedData, &encodedParity, &encodedFragmentLen)
	if rc != 0 {
		return nil, errors.New("liberasurecode encode failed")
	}

	defer C.liberasurecode_encode_cleanup(C.int(desc), encodedData, encodedParity)

	numChunks := int(args.k + args.m)
	chunks := make([][]byte, numChunks)

	for i := 0; i < numChunks; i++ {
		var frag *C.char
		if i < int(args.k) {
			frag = encodedData[i]
		} else {
			frag = encodedParity[i-int(args.k)]
		}
		chunks[i] = C.GoBytes(unsafe.Pointer(frag), C.int(encodedFragmentLen))
	}

	return chunks, nil
}

// Decode performs data decoding using liberasurecode.
func Decode(chunks [][]byte, args *ECArgs, desc int) ([]byte, error) {
	var availFrags **C.char
	var decodedData *C.char
	var decodedDataLen C.uint64_t

	numChunks := int(args.k + args.m)
	availFrags = (**C.char)(C.malloc(C.size_t(numChunks) * sizeof_char_ptr))
	if availFrags == nil {
		return nil, errors.New("malloc failed")
	}
	defer C.free(unsafe.Pointer(availFrags))

	for i := 0; i < numChunks; i++ {
		C.set_char_pp(availFrags, chunks[i])
	}

	rc := C.liberasurecode_decode(C.int(desc), availFrags, C.int(numChunks), C.size_t(len(chunks[0])), 1,
		&decodedData, &decodedDataLen)
	if rc != 0 {
		return nil, errors.New("liberasurecode decode failed")
	}

	defer C.liberasurecode_decode_cleanup(C.int(desc), decodedData)

	return C.GoBytes(unsafe.Pointer(decodedData), C.int(decodedDataLen)), nil
}