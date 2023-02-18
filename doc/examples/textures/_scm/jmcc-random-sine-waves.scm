; random sine waves (jmcc) #1
(withSc3
 (overlapTextureUgen
  (list 2 5 12 +inf.0)
  (Pan2 (FSinOsc (Rand 10 2000) 0) (Rand -1 1) 0.02)))
