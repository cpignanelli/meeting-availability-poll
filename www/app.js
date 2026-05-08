(function () {
  var ORGANIZER_NAME_KEY = "meetingAvailabilityPoll.organizerName";
  var ORGANIZER_EMAIL_KEY = "meetingAvailabilityPoll.organizerEmail";
  var shinyHandlersRegistered = false;
  var calendarDragState = null;
  var suppressNextSlotClick = false;
  var pendingCalendarInput = null;
  var pendingCalendarChanges = new Map();
  var pendingCalendarFlushTimer = null;
  var pendingCalendarMaxTimer = null;
  var availabilityCycleStates = ["pending", "available", "preferred", "unavailable"];
  var availabilityCycleValues = {
    pending: "",
    available: "available",
    preferred: "preferred",
    unavailable: "unavailable"
  };
  var availabilityCycleLabels = {
    pending: "Pending",
    available: "Available",
    preferred: "Preferred",
    unavailable: "Unavailable"
  };
  var availabilityCycleHints = {
    pending: "Not answered yet",
    available: "I can attend",
    preferred: "Best for me",
    unavailable: "I cannot attend"
  };
  var availabilityCycleIcons = {
    pending: "○",
    available: "✓",
    preferred: "★",
    unavailable: "×"
  };

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

  function findCreateInput(name) {
    return document.querySelector("[id$='-" + name + "']");
  }

  function setInputValue(input, value) {
    if (!input) {
      return;
    }
    input.value = value || "";
    input.dispatchEvent(new Event("input", { bubbles: true }));
    input.dispatchEvent(new Event("change", { bubbles: true }));
  }

  function readOrganizerFields() {
    var nameInput = findCreateInput("organizer_name");
    var emailInput = findCreateInput("organizer_email");
    return {
      name: nameInput ? nameInput.value.trim() : "",
      email: emailInput ? emailInput.value.trim() : ""
    };
  }

  function saveOrganizerDetails() {
    var rememberInput = findCreateInput("remember_organizer");
    if (!rememberInput || !rememberInput.checked) {
      return;
    }
    var fields = readOrganizerFields();
    try {
      window.localStorage.setItem(ORGANIZER_NAME_KEY, fields.name);
      window.localStorage.setItem(ORGANIZER_EMAIL_KEY, fields.email);
    } catch (error) {
      // Local storage can be unavailable in strict browser privacy modes.
    }
  }

  function restoreOrganizerDetails() {
    var nameInput = findCreateInput("organizer_name");
    var emailInput = findCreateInput("organizer_email");
    var rememberInput = findCreateInput("remember_organizer");
    if (!nameInput || !emailInput || !rememberInput) {
      return;
    }

    try {
      var savedName = window.localStorage.getItem(ORGANIZER_NAME_KEY) || "";
      var savedEmail = window.localStorage.getItem(ORGANIZER_EMAIL_KEY) || "";
      if (!savedName && !savedEmail) {
        return;
      }
      if (!nameInput.value) {
        setInputValue(nameInput, savedName);
      }
      if (!emailInput.value) {
        setInputValue(emailInput, savedEmail);
      }
      rememberInput.checked = true;
      rememberInput.dispatchEvent(new Event("change", { bubbles: true }));
    } catch (error) {
      // Ignore storage read errors and leave the form empty.
    }
  }

  function clearOrganizerDetails() {
    try {
      window.localStorage.removeItem(ORGANIZER_NAME_KEY);
      window.localStorage.removeItem(ORGANIZER_EMAIL_KEY);
    } catch (error) {
      // Ignore storage write errors.
    }
    setInputValue(findCreateInput("organizer_name"), "");
    setInputValue(findCreateInput("organizer_email"), "");
    var rememberInput = findCreateInput("remember_organizer");
    if (rememberInput) {
      rememberInput.checked = false;
      rememberInput.dispatchEvent(new Event("change", { bubbles: true }));
    }
  }

  function setCalendarSlotSelected(button, selected) {
    if (!button || button.disabled) {
      return;
    }
    button.classList.toggle("calendar-slot-selected", selected);
    button.setAttribute("aria-pressed", selected ? "true" : "false");
    button.textContent = selected ? "Selected" : "+";
  }

  function clearCalendarFlushTimers() {
    if (pendingCalendarFlushTimer) {
      window.clearTimeout(pendingCalendarFlushTimer);
      pendingCalendarFlushTimer = null;
    }
    if (pendingCalendarMaxTimer) {
      window.clearTimeout(pendingCalendarMaxTimer);
      pendingCalendarMaxTimer = null;
    }
  }

  function flushCalendarSlotChanges() {
    if (!window.Shiny || !pendingCalendarInput || pendingCalendarChanges.size === 0) {
      clearCalendarFlushTimers();
      return;
    }
    var inputId = pendingCalendarInput;
    var changes = Array.from(pendingCalendarChanges.values());
    pendingCalendarInput = null;
    pendingCalendarChanges.clear();
    clearCalendarFlushTimers();
    window.Shiny.setInputValue(
      inputId,
      {
        changes: changes,
        nonce: Date.now() + ":" + Math.random()
      },
      { priority: "event" }
    );
  }

  function queueCalendarSlotChange(button, selected, delay) {
    if (!window.Shiny || !button) {
      return;
    }
    var inputId = button.getAttribute("data-shiny-input");
    if (pendingCalendarInput && pendingCalendarInput !== inputId) {
      flushCalendarSlotChanges();
    }
    pendingCalendarInput = inputId;
    pendingCalendarChanges.set(button.getAttribute("data-slot-key"), {
      key: button.getAttribute("data-slot-key"),
      selected: !!selected
    });

    if (pendingCalendarFlushTimer) {
      window.clearTimeout(pendingCalendarFlushTimer);
    }
    pendingCalendarFlushTimer = window.setTimeout(flushCalendarSlotChanges, delay || 80);
    if (!pendingCalendarMaxTimer) {
      pendingCalendarMaxTimer = window.setTimeout(flushCalendarSlotChanges, 320);
    }
  }

  function applyCalendarSlotChange(button, selected, delay) {
    setCalendarSlotSelected(button, selected);
    queueCalendarSlotChange(button, selected, delay);
  }

  function calendarSlotFromEvent(event) {
    var slotButton = event.target.closest("[data-slot-key]");
    if (!slotButton || slotButton.disabled) {
      return null;
    }
    return slotButton;
  }

  function findCalendarTimeTarget(container, time) {
    var targets = container.querySelectorAll("[data-time-row], [data-start-time]");
    for (var i = 0; i < targets.length; i += 1) {
      if (targets[i].getAttribute("data-time-row") === time || targets[i].getAttribute("data-start-time") === time) {
        return targets[i];
      }
    }
    return null;
  }

  function scrollCalendarToTime(containerId, time) {
    var container = document.getElementById(containerId);
    if (!container) {
      return false;
    }
    var target = findCalendarTimeTarget(container, time);
    if (!target) {
      return false;
    }
    var header = container.querySelector(".calendar-day-header");
    var containerRect = container.getBoundingClientRect();
    var targetRect = target.getBoundingClientRect();
    var offset = header ? header.getBoundingClientRect().height : 0;
    var targetTop = Math.max(container.scrollTop + targetRect.top - containerRect.top - offset, 0);
    container.scrollTop = targetTop;
    var updatedTargetRect = target.getBoundingClientRect();
    var expectedTop = container.getBoundingClientRect().top + offset;
    return Math.abs(updatedTargetRect.top - expectedTop) < 4;
  }

  function scheduleCalendarScrollToTime(containerId, time) {
    [0, 16, 50, 120, 250, 500, 900].forEach(function (delay) {
      window.setTimeout(function () {
        window.requestAnimationFrame(function () {
          scrollCalendarToTime(containerId, time);
        });
      }, delay);
    });
  }

  function syncCalendarSelection(message) {
    var container = document.getElementById(message.container_id);
    if (!container) {
      return;
    }
    if (pendingCalendarChanges.size > 0) {
      return;
    }
    var selected = new Set(message.selected || []);
    container.querySelectorAll("[data-slot-key]").forEach(function (button) {
      setCalendarSlotSelected(button, selected.has(button.getAttribute("data-slot-key")));
    });
  }

  function normalizeAvailabilityState(state) {
    return availabilityCycleStates.indexOf(state) >= 0 ? state : "pending";
  }

  function setAvailabilityCycleState(button, state) {
    if (!button) {
      return;
    }
    state = normalizeAvailabilityState(state);
    availabilityCycleStates.forEach(function (cycleState) {
      button.classList.remove("availability-state-" + cycleState);
    });
    button.classList.add("availability-state-" + state);
    button.setAttribute("data-availability-state", state);
    button.setAttribute("data-availability-value", availabilityCycleValues[state]);
    button.setAttribute("aria-pressed", state === "pending" ? "false" : "true");
    button.setAttribute("aria-label", availabilityCycleLabels[state] + ". Activate to change response.");

    var icon = button.querySelector(".availability-cycle-icon");
    var label = button.querySelector(".availability-cycle-label");
    var hint = button.querySelector(".availability-cycle-hint");
    if (icon) {
      icon.textContent = availabilityCycleIcons[state];
    }
    if (label) {
      label.textContent = availabilityCycleLabels[state];
    }
    if (hint) {
      hint.textContent = availabilityCycleHints[state];
    }
  }

  function cycleAvailabilityButton(button) {
    var currentState = normalizeAvailabilityState(button.getAttribute("data-availability-state"));
    var currentIndex = availabilityCycleStates.indexOf(currentState);
    var nextState = availabilityCycleStates[(currentIndex + 1) % availabilityCycleStates.length];
    setAvailabilityCycleState(button, nextState);
    if (window.Shiny) {
      window.Shiny.setInputValue(
        button.getAttribute("data-availability-input"),
        availabilityCycleValues[nextState],
        { priority: "event" }
      );
    }
  }

  function registerShinyHandlers() {
    if (!window.Shiny || shinyHandlersRegistered) {
      return;
    }
    shinyHandlersRegistered = true;
    window.Shiny.addCustomMessageHandler("calendarSelection", syncCalendarSelection);
    window.Shiny.addCustomMessageHandler("calendarScrollToTime", function (message) {
      scheduleCalendarScrollToTime(message.container_id, message.time || "08:00");
    });
  }

  document.addEventListener("pointerdown", function (event) {
    var slotButton = calendarSlotFromEvent(event);
    if (!slotButton || event.button !== 0 || event.pointerType === "touch") {
      return;
    }

    event.preventDefault();
    var selected = !slotButton.classList.contains("calendar-slot-selected");
    calendarDragState = {
      selected: selected,
      seen: new Set()
    };
    suppressNextSlotClick = true;
    window.setTimeout(function () {
      suppressNextSlotClick = false;
    }, 500);

    calendarDragState.seen.add(slotButton.getAttribute("data-slot-key"));
    applyCalendarSlotChange(slotButton, selected, 160);
  });

  document.addEventListener("pointerover", function (event) {
    if (!calendarDragState) {
      return;
    }
    var slotButton = calendarSlotFromEvent(event);
    if (!slotButton) {
      return;
    }
    var key = slotButton.getAttribute("data-slot-key");
    if (calendarDragState.seen.has(key)) {
      return;
    }
    calendarDragState.seen.add(key);
    applyCalendarSlotChange(slotButton, calendarDragState.selected, 160);
  });

  document.addEventListener("pointerup", function () {
    flushCalendarSlotChanges();
    calendarDragState = null;
  });

  document.addEventListener("pointercancel", function () {
    flushCalendarSlotChanges();
    calendarDragState = null;
    suppressNextSlotClick = false;
  });

  window.addEventListener("blur", function () {
    flushCalendarSlotChanges();
    calendarDragState = null;
    suppressNextSlotClick = false;
  });

  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-copy-target]");
    if (button) {
      event.preventDefault();
      copyTextFromTarget(button.getAttribute("data-copy-target"), button);
      return;
    }

    var slotButton = calendarSlotFromEvent(event);
    if (slotButton) {
      event.preventDefault();
      if (suppressNextSlotClick) {
        suppressNextSlotClick = false;
        return;
      }
      applyCalendarSlotChange(slotButton, !slotButton.classList.contains("calendar-slot-selected"), 20);
      flushCalendarSlotChanges();
      return;
    }

    var cycleButton = event.target.closest("[data-availability-cycle]");
    if (cycleButton) {
      event.preventDefault();
      cycleAvailabilityButton(cycleButton);
      return;
    }

    if (event.target.closest("[id$='-clear_saved_organizer']")) {
      event.preventDefault();
      clearOrganizerDetails();
    }
    if (event.target.closest("[id$='-create_poll']")) {
      saveOrganizerDetails();
    }
  });

  document.addEventListener("input", function (event) {
    if (event.target.matches("[id$='-organizer_name'], [id$='-organizer_email']")) {
      saveOrganizerDetails();
    }
  });

  document.addEventListener("change", function (event) {
    if (event.target.matches("[id$='-remember_organizer']")) {
      if (event.target.checked) {
        saveOrganizerDetails();
      }
    }
  });

  document.addEventListener("DOMContentLoaded", function () {
    restoreOrganizerDetails();
    registerShinyHandlers();
  });
  document.addEventListener("shiny:bound", restoreOrganizerDetails);
  document.addEventListener("shiny:connected", registerShinyHandlers);
  registerShinyHandlers();
})();
