(function () {
  function copyTextFromTarget(targetId, button) {
    var target = document.getElementById(targetId);
    if (!target) {
      return;
    }

    var text = target.value || target.textContent || "";
    var originalText = button.textContent;

    function markCopied() {
      button.textContent = "Copied";
      window.setTimeout(function () {
        button.textContent = originalText;
      }, 1400);
    }

    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(markCopied).catch(function () {
        target.focus();
        target.select();
      });
      return;
    }

    target.focus();
    if (target.select) {
      target.select();
    }
    try {
      document.execCommand("copy");
      markCopied();
    } catch (error) {
      // Keep the text selected so the user can copy manually.
    }
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-copy-target]");
    if (!button) {
      return;
    }
    event.preventDefault();
    copyTextFromTarget(button.getAttribute("data-copy-target"), button);
  });
})();
