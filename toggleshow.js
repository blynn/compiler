<script>
"use strict";
const its = document.getElementsByClassName("toggleshow");
for (const it of its) {
  const toggler = document.createElement("p");
  const a = document.createElement("a");
  toggler.appendChild(a);
  const foldedMsg = "&blacktriangleright; <code>" + it.id + "</code>";
  const unfoldedMsg = "&blacktriangledown; <code>" + it.id + "</code>";
  a.innerHTML = foldedMsg;
  it.style.display = "none";
  it.parentNode.insertBefore(toggler, it);
  a.addEventListener("click", (event) => {
    if (it.style.display == "none") {
      it.style.display = "block";
      a.innerHTML = unfoldedMsg;
    } else {
      it.style.display = "none";
      a.innerHTML = foldedMsg;
    }
  });
}
</script>
