// TODO: The tactic part seem to be unimplemented now.

function filterSelectionClass(tagNames, classname) {
  if (tagNames.length == 0) {
    for (const elem of document.getElementsByClassName(classname)) {
      elem.classList.remove("hide");
    }
  } else {
    // Add the "show" class (display:block) to the filtered elements, and remove the "show" class from the elements that are not selected
    for (const elem of document.getElementsByClassName(classname)) {
      elem.classList.add("hide");
      for (const tagName of tagNames) {
        if (elem.classList.contains(tagName)) {
          elem.classList.remove("hide");
        }
      }
    }
  }
}

function filterSelection(c) {
  filterSelectionClass(c, "tactic");
  filterSelectionClass(c, "taclink");
}

var filterBoxes = document.getElementsByClassName("tagfilter");

function updateDisplay() {
  filterSelection(getSelectValues());
}

function getSelectValues() {
  var result = [];

  for (const opt of filterBoxes) {
    if (opt.checked) {
      result.push(opt.value);
    }
  }
  return result;
}

function setSelectVal(val) {
  for (const opt of filterBoxes) {
    opt.checked = val;
  }
}

updateDisplay();

for (const opt of filterBoxes) {
  opt.addEventListener("change", updateDisplay);
}

const tse = document.getElementById("tagfilter-selectall");
if (tse != null) {
  tse.addEventListener("change", function () {
    setSelectVal(this.checked);
    updateDisplay();
  });
}
