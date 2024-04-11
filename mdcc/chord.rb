

class Chord
	KeySpace = 256
	MaximumKeyLength = (KeySpace / 4) - 1
	KeyBitMask = (1 << KeySpace) - 1

	attr_header :origin

	def initialize(*args)
	end

	def join(node)
	end

	def query(id)
	end

end
s