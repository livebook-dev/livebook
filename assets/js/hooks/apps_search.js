/**
 * A hook for client-side app filtering to avoid server round-trips.
 *
 * This hook filters app cards in real-time based on search input and folder selection.
 */
const AppsSearch = {
  mounted() {
    this.searchInput = this.el.querySelector("#search-app");
    this.folderSelect = this.el.querySelector("#select-app-folder");
    this.noResultsMessage = this.el.querySelector("[data-no-results]");

    this.searchInput.addEventListener("input", () => {
      this.applyFilters();
    });

    if (this.folderSelect) {
      this.folderSelect.addEventListener("change", () => {
        this.applyFilters();
      });
    }

    this.refreshElements();
    this.applyFilters();
  },

  updated() {
    this.refreshElements();
    this.applyFilters();
  },

  refreshElements() {
    this.appCards = Array.from(this.el.querySelectorAll("[data-app-card]"));
    this.appGroups = Array.from(this.el.querySelectorAll("[data-app-group]"));
  },

  applyFilters() {
    const searchTerm = this.searchInput.value.toLowerCase().trim();
    const selectedFolder = this.folderSelect ? this.folderSelect.value : "";

    let visibleCount = 0;

    this.appCards.forEach((card) => {
      const appName = card.dataset.appName.toLowerCase();
      const appSlug = card.dataset.appSlug.toLowerCase();
      const appFolderId = card.dataset.appFolderId || "";

      const matchesSearch =
        searchTerm === "" ||
        appName.includes(searchTerm) ||
        appSlug.includes(searchTerm);

      const matchesFolder =
        selectedFolder === "" || appFolderId === selectedFolder;

      if (matchesSearch && matchesFolder) {
        card.style.display = "";
        visibleCount++;
      } else {
        card.style.display = "none";
      }
    });

    this.appGroups.forEach((group) => {
      const visibleCardsInGroup = group.querySelectorAll(
        "[data-app-card]:not([style*='display: none'])",
      );
      const count = visibleCardsInGroup.length;

      if (count === 0) {
        group.style.display = "none";
      } else {
        group.style.display = "";
        const countElement = group.querySelector("[data-group-count]");
        if (countElement) {
          countElement.textContent = `(${count})`;
        }
      }
    });

    if (this.noResultsMessage) {
      this.noResultsMessage.style.display = visibleCount === 0 ? "" : "none";
    }
  },
};

export default AppsSearch;
