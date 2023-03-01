// Index persistence

local indices gender_index2 aspiration_index2 behavior_index2

foreach ind in  `indices' {
	corr B_S`ind' E_S`ind' E2_S`ind' if B_treat==0
	}