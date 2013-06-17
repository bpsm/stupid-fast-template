(ns us.bpsm.sft.lorem
  (:require [us.bpsm.sft :as sft]))

(def lorem*
  (sft/template-fn* :rsrc "us/bpsm/sft/lorem.txt"))

(def lorem
  (sft/template-fn :rsrc "us/bpsm/sft/lorem.txt"))

(def props {:Name "Mauris", :name "mauris"})
