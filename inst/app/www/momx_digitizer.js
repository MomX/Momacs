/* ============================================================================
   MOMACS DIGITIZER - Canvas-based Morphometric Digitization
   
   This JavaScript component handles all canvas interactions for digitizing
   morphometric data including landmarks, polylines, polygons, and curves.
   
   Features:
   - Interactive canvas with zoom/pan
   - Multiple digitization modes (landmarks, polylines, polygons, curves)
   - Catmull-Rom curve interpolation
   - Undo/redo with history management
   - Point manipulation (drag, delete, insert)
   - Keyboard shortcuts
   ============================================================================ */

// ============================================================================
// MODAL STATE HANDLERS
// ============================================================================

/**
 * Handle comment modal opening
 * Disables keyboard shortcuts while modal is open
 */
Shiny.addCustomMessageHandler('comment_modal_opening', function(data) {
  if (digitizer) {
    digitizer.modalOpen = true;
  }
});

/**
 * Handle comment modal closing
 * Re-enables keyboard shortcuts
 */
Shiny.addCustomMessageHandler('comment_modal_closing', function(data) {
  if (digitizer) {
    digitizer.modalOpen = false;
  }
});

// ============================================================================
// MAIN DIGITIZE CANVAS CLASS
// ============================================================================

/**
 * DigitizeCanvas - Main class for canvas-based digitization
 * 
 * Manages all canvas interactions, rendering, and data manipulation
 * for morphometric digitization.
 */
class DigitizeCanvas {
  /**
   * Initialize the digitization canvas
   * @param {string} canvasId - ID of the canvas element
   */
  constructor(canvasId) {
    // Canvas setup
    this.canvas = document.getElementById(canvasId);
    this.ctx = this.canvas.getContext('2d');
    this.image = null;
    
    // Data structure for digitized points
    this.digitizedData = {
      landmarks: [],
      polylines_open: [],
      polylines_closed: [],
      curves_open: [],
      curves_closed: []
    };
    
    // UI state
    this.mode = 'landmarks';           // Current digitization mode
    this.zoom = 1;                     // Current zoom level
    this.panX = 0;                     // Pan offset X
    this.panY = 0;                     // Pan offset Y
    this.zoomInitial = 1;              // Initial zoom (for reset)
    this.panXInitial = 0;              // Initial pan X (for reset)
    this.panYInitial = 0;              // Initial pan Y (for reset)
    
    // Interaction state
    this.isDragging = false;           // Currently dragging a point
    this.isPanning = false;            // Currently panning view
    this.spaceDown = false;            // Space key pressed
    this.dragStartX = 0;               // Drag start position X
    this.dragStartY = 0;               // Drag start position Y
    this.lastMouseX = 0;               // Last mouse position X
    this.lastMouseY = 0;               // Last mouse position Y
    this.editingPoint = null;          // Point being edited
    this.hoverPoint = null;            // Point under cursor
    this.hoverSegment = null;          // Segment under cursor (for insertion)
    
    // Right-click handling for point deletion
    this.rightClickStartTime = 0;      // Time right-click started
    this.rightClickDuration = 800;     // Hold duration for partition delete
    this.rightClickTimer = null;       // Timer for hold detection
    this.rightClickPoint = null;       // Point right-clicked
    
    // Undo/redo history
    this.history = [];                 // History stack
    this.historyIndex = -1;            // Current position in history
    this.maxHistorySize = 50;          // Maximum history entries
    
    // Modal state
    this.modalOpen = false;            // Comment modal open flag
    
    // Initialize
    this.setupEventListeners();
    this.resizeCanvas();
  }
  
  // ==========================================================================
  // EVENT LISTENERS
  // ==========================================================================
  
  /**
   * Set up all canvas and document event listeners
   */
  setupEventListeners() {
    // Canvas events
    this.canvas.addEventListener('click', (e) => this.handleClick(e));
    this.canvas.addEventListener('contextmenu', (e) => this.handleContextMenu(e));
    this.canvas.addEventListener('wheel', (e) => this.handleZoom(e), { passive: false });
    this.canvas.addEventListener('mousedown', (e) => this.handleMouseDown(e));
    this.canvas.addEventListener('mousemove', (e) => this.handleMouseMove(e));
    this.canvas.addEventListener('mouseup', (e) => this.handleMouseUp(e));
    this.canvas.addEventListener('mouseleave', (e) => this.handleMouseUp(e));
    
    // Document events for keyboard shortcuts
    document.addEventListener('keydown', (e) => this.handleKeyDown(e));
    document.addEventListener('keyup', (e) => this.handleKeyUp(e));
    
    // Window resize
    window.addEventListener('resize', () => this.resizeCanvas());
  }
  
  /**
   * Resize canvas to fit container while maintaining aspect ratio
   */
  resizeCanvas() {
    const container = this.canvas.parentElement;
    const maxWidth = Math.min(container.clientWidth - 40, window.innerWidth * 0.9);
    const maxHeight = Math.min(600, window.innerHeight * 0.7);
    
    this.canvas.width = Math.max(600, maxWidth);
    this.canvas.height = Math.max(400, maxHeight);
    this.redraw();
  }
  
  // ==========================================================================
  // COORDINATE TRANSFORMATION
  // ==========================================================================
  
  /**
   * Convert canvas coordinates to image coordinates
   * @param {number} canvasX - Canvas X coordinate
   * @param {number} canvasY - Canvas Y coordinate
   * @returns {Object} Object with x, y in image space
   */
  canvasToImage(canvasX, canvasY) {
    return {
      x: (canvasX - this.panX) / this.zoom,
      y: (canvasY - this.panY) / this.zoom
    };
  }
  
  /**
   * Convert image coordinates to canvas coordinates
   * @param {number} imageX - Image X coordinate
   * @param {number} imageY - Image Y coordinate
   * @returns {Object} Object with x, y in canvas space
   */
  imageToCanvas(imageX, imageY) {
    return {
      x: imageX * this.zoom + this.panX,
      y: imageY * this.zoom + this.panY
    };
  }
  
  // ==========================================================================
  // LOGGING AND COMMUNICATION
  // ==========================================================================
  
  /**
   * Log action text to Shiny action bar
   * @param {string} actionText - Text to display
   */
  logAction(actionText) {
    Shiny.setInputValue('action_status', actionText);
  }
  
  // ==========================================================================
  // CURVE INTERPOLATION - CATMULL-ROM
  // ==========================================================================
  
  /**
   * Calculate a point on a centripetal Catmull-Rom curve
   * @param {Object} p0 - Control point 0
   * @param {Object} p1 - Control point 1
   * @param {Object} p2 - Control point 2
   * @param {Object} p3 - Control point 3
   * @param {number} t - Parameter (0-1)
   * @param {number} alpha - Curve tension (default 0.5 for centripetal)
   * @returns {Object} Interpolated point
   */
  catmullRomPoint(p0, p1, p2, p3, t, alpha = 0.5) {
    // Calculate segment distances
    const d01 = Math.pow(Math.hypot(p1.x - p0.x, p1.y - p0.y), alpha);
    const d12 = Math.pow(Math.hypot(p2.x - p1.x, p2.y - p1.y), alpha);
    const d23 = Math.pow(Math.hypot(p3.x - p2.x, p3.y - p2.y), alpha);
    
    const eps = 1e-6;  // Small value to prevent division by zero
    
    // Calculate knot values
    const t0 = 0;
    const t1 = d01 + t0;
    const t2 = d12 + t1;
    const t3 = d23 + t2;
    const t_eval = t1 + t * (t2 - t1);
    
    // First level of interpolation
    const A1 = { 
      x: (t1 - t_eval) / (t1 - t0 + eps) * p0.x + (t_eval - t0) / (t1 - t0 + eps) * p1.x,
      y: (t1 - t_eval) / (t1 - t0 + eps) * p0.y + (t_eval - t0) / (t1 - t0 + eps) * p1.y 
    };
    const A2 = { 
      x: (t2 - t_eval) / (t2 - t1 + eps) * p1.x + (t_eval - t1) / (t2 - t1 + eps) * p2.x,
      y: (t2 - t_eval) / (t2 - t1 + eps) * p1.y + (t_eval - t1) / (t2 - t1 + eps) * p2.y 
    };
    const A3 = { 
      x: (t3 - t_eval) / (t3 - t2 + eps) * p2.x + (t_eval - t2) / (t3 - t2 + eps) * p3.x,
      y: (t3 - t_eval) / (t3 - t2 + eps) * p2.y + (t_eval - t2) / (t3 - t2 + eps) * p3.y 
    };
    
    // Second level of interpolation
    const B1 = { 
      x: (t2 - t_eval) / (t2 - t0 + eps) * A1.x + (t_eval - t0) / (t2 - t0 + eps) * A2.x,
      y: (t2 - t_eval) / (t2 - t0 + eps) * A1.y + (t_eval - t0) / (t2 - t0 + eps) * A2.y 
    };
    const B2 = { 
      x: (t3 - t_eval) / (t3 - t1 + eps) * A2.x + (t_eval - t1) / (t3 - t1 + eps) * A3.x,
      y: (t3 - t_eval) / (t3 - t1 + eps) * A2.y + (t_eval - t1) / (t3 - t1 + eps) * A3.y 
    };
    
    // Final interpolation
    return {
      x: (t2 - t_eval) / (t2 - t1 + eps) * B1.x + (t_eval - t1) / (t2 - t1 + eps) * B2.x,
      y: (t2 - t_eval) / (t2 - t1 + eps) * B1.y + (t_eval - t1) / (t2 - t1 + eps) * B2.y
    };
  }
  
  /**
   * Generate smooth curve points from control points
   * @param {Array} coords - Array of control points
   * @param {boolean} isClosed - Whether curve is closed
   * @returns {Array} Array of interpolated curve points
   */
  generateCurvePoints(coords, isClosed) {
    if (!coords || coords.length < 2) return coords;
    if (coords.length === 2) return coords;
    
    const points = [];
    const resolution = 20;  // Points between each control point
    
    // Always include first control point
    points.push({x: coords[0].x, y: coords[0].y, is_corner: coords[0].is_corner});
    
    // Interpolate between consecutive control points
    for (let i = 0; i < coords.length - 1; i++) {
      const curr = coords[i];
      const next = coords[i + 1];
      
      // Skip interpolation if current point is a corner
      if (curr.is_corner) {
        points.push({x: next.x, y: next.y, is_corner: next.is_corner});
        continue;
      }
      
      // Get surrounding control points for Catmull-Rom
      let p0, p3;
      if (isClosed) {
        p0 = i === 0 ? coords[coords.length - 1] : coords[i - 1];
        p3 = i === coords.length - 2 ? coords[0] : coords[i + 2];
      } else {
        // For open curves, extrapolate end points
        p0 = i === 0 ? {x: 2 * curr.x - next.x, y: 2 * curr.y - next.y} : coords[i - 1];
        p3 = i === coords.length - 2 ? {x: 2 * next.x - curr.x, y: 2 * next.y - curr.y} : coords[i + 2];
      }
      
      // Generate interpolated points
      for (let j = 1; j < resolution; j++) {
        const t = j / resolution;
        const pt = this.catmullRomPoint(p0, curr, next, p3, t);
        points.push(pt);
      }
      
      // Include next control point
      points.push({x: next.x, y: next.y, is_corner: next.is_corner});
    }
    
    return points;
  }
  
  // ==========================================================================
  // MOUSE EVENT HANDLERS
  // ==========================================================================
  
  /**
   * Handle canvas click (add point or insert point on segment)
   */
  handleClick(e) {
    if (this.isPanning || this.isDragging) return;
    
    const rect = this.canvas.getBoundingClientRect();
    const canvasX = e.clientX - rect.left;
    const canvasY = e.clientY - rect.top;
    
    const pointHit = this.getPointAtCanvas(canvasX, canvasY);
    const imgCoords = this.canvasToImage(canvasX, canvasY);
    const segmentHit = this.getSegmentAtCanvas(canvasX, canvasY);
    
    // Insert point on segment if hovering over one
    if (segmentHit) {
      this.logAction(`Inserted point on ${segmentHit.data_type}`);
      Shiny.setInputValue('insert_point', {
        data_type: segmentHit.data_type,
        partition_id: segmentHit.partition_id,
        insert_idx: segmentHit.insert_idx,
        x: segmentHit.x,
        y: segmentHit.y,
        is_corner: false,
        timestamp: Date.now()
      });
      return;
    }
    
    // Validate point is within image bounds
    if (!this.isPointInBounds(imgCoords.x, imgCoords.y)) {
      this.logAction('⚠ Point outside image bounds • please click within image');
      return;
    }
    
    // Add new point
    const typeLabel = this.getModeLabel(this.mode);
    this.logAction(`Added point to ${typeLabel}`);
    Shiny.setInputValue('canvas_click', {
      x: imgCoords.x,
      y: imgCoords.y,
      timestamp: Date.now()
    });
    
    // Update action text with point details
    setTimeout(() => {
      const lastPoint = this.getLastPointLabel();
      if (lastPoint) {
        this.logAction(`Defined ${lastPoint.label} at (${lastPoint.x}, ${lastPoint.y}) • awaiting ${lastPoint.nextLabel}`);
      }
    }, 50);
  }
  
  /**
   * Check if point is within image bounds
   */
  isPointInBounds(x, y) {
    if (!this.image) return true;
    return x >= 0 && x <= this.image.width && y >= 0 && y <= this.image.height;
  }
  
  /**
   * Prevent context menu on right-click
   */
  handleContextMenu(e) {
    e.preventDefault();
  }
  
  /**
   * Handle mouse down (start drag or pan)
   */
  handleMouseDown(e) {
    const rect = this.canvas.getBoundingClientRect();
    const canvasX = e.clientX - rect.left;
    const canvasY = e.clientY - rect.top;
    
    // Left click: start dragging point if over one
    if (e.button === 0) {
      const pointHit = this.getPointAtCanvas(canvasX, canvasY);
      if (pointHit && !this.isPanning) {
        this.editingPoint = pointHit;
        this.isDragging = true;
        this.logAction(`Editing point ${this.getPointLabel(pointHit)}`);
        return;
      }
    }
    
    // Right click: delete point or start hold timer for partition delete
    if (e.button === 2) {
      e.preventDefault();
      const pointHit = this.getPointAtCanvas(canvasX, canvasY);
      
      if (pointHit) {
        // Shift+right-click: delete entire partition immediately
        if (e.shiftKey) {
          const pt = pointHit;
          this.logAction(`Deleted entire partition ${this.getPointLabel(pt).split('.')[0]}`);
          Shiny.setInputValue('delete_partition_by_point', {
            data_type: pt.data_type,
            partition_id: pt.partition_id,
            timestamp: Date.now()
          });
          return;
        }
        
        // Normal right-click: start timer for hold-to-delete partition
        this.rightClickStartTime = Date.now();
        this.rightClickPoint = pointHit;
        
        this.rightClickTimer = setTimeout(() => {
          if (this.rightClickPoint) {
            const pt = this.rightClickPoint;
            this.logAction(`Deleted entire partition ${this.getPointLabel(pt).split('.')[0]}`);
            Shiny.setInputValue('delete_partition_by_point', {
              data_type: pt.data_type,
              partition_id: pt.partition_id,
              timestamp: Date.now()
            });
            this.rightClickPoint = null;
          }
        }, this.rightClickDuration);
      } else {
        // Right-click on empty space: start panning
        this.isPanning = true;
        this.dragStartX = canvasX - this.panX;
        this.dragStartY = canvasY - this.panY;
      }
    }
  }
  
  /**
   * Handle mouse move (drag point, pan, or update hover)
   */
  handleMouseMove(e) {
    const rect = this.canvas.getBoundingClientRect();
    const canvasX = e.clientX - rect.left;
    const canvasY = e.clientY - rect.top;
    const imgCoords = this.canvasToImage(canvasX, canvasY);
    
    // Send cursor position to Shiny
    if (this.image) {
      Shiny.setInputValue('canvas_stats', {
        imageWidth: this.image.width,
        imageHeight: this.image.height,
        cursorX: Math.round(imgCoords.x),
        cursorY: Math.round(imgCoords.y),
        zoom: (this.zoom * 100).toFixed(0)
      });
    }
    
    // Update hover state
    this.hoverPoint = this.getPointAtCanvas(canvasX, canvasY);
    if (!this.isDragging) {
      this.hoverSegment = this.getSegmentAtCanvas(canvasX, canvasY);
    }
    
    // Dragging a point
    if (this.isDragging && this.editingPoint) {
      const ep = this.editingPoint;
      const list = this.digitizedData[ep.data_type];
      
      if (list && list[ep.partition_id - 1]) {
        const partition = list[ep.partition_id - 1];
        if (partition.coords[ep.point_idx - 1]) {
          // Update point position
          partition.coords[ep.point_idx - 1].x = imgCoords.x;
          partition.coords[ep.point_idx - 1].y = imgCoords.y;
          this.redraw();
          
          // Send to Shiny
          Shiny.setInputValue('update_point', {
            data_type: ep.data_type,
            partition_id: ep.partition_id,
            point_idx: ep.point_idx,
            x: imgCoords.x,
            y: imgCoords.y,
            timestamp: Date.now()
          });
        }
      }
      return;
    }
    
    // Panning with space key
    if (this.isPanning && this.spaceDown) {
      this.panX += canvasX - (this.lastMouseX || canvasX);
      this.panY += canvasY - (this.lastMouseY || canvasY);
    } 
    // Panning with right-click drag
    else if (this.isPanning && !this.spaceDown) {
      this.panX = canvasX - this.dragStartX;
      this.panY = canvasY - this.dragStartY;
    }
    
    this.lastMouseX = canvasX;
    this.lastMouseY = canvasY;
    this.redraw();
  }
  
  /**
   * Handle mouse up (stop drag/pan, handle quick right-click)
   */
  handleMouseUp(e) {
    this.isDragging = false;
    this.isPanning = false;
    this.editingPoint = null;
    
    // Handle quick right-click (delete single point)
    if (this.rightClickTimer) {
      clearTimeout(this.rightClickTimer);
      this.rightClickTimer = null;
      
      if (this.rightClickPoint && Date.now() - this.rightClickStartTime < this.rightClickDuration) {
        const pt = this.rightClickPoint;
        const partId = pt.partition_id;
        const dataType = pt.data_type;
        this.logAction(`Deleted point ${this.getPointLabel(pt)}`);
        Shiny.setInputValue('delete_point', {
          data_type: dataType,
          partition_id: partId,
          point_idx: pt.point_idx,
          timestamp: Date.now()
        });
      }
      this.rightClickPoint = null;
    }
  }
  
  // ==========================================================================
  // KEYBOARD EVENT HANDLERS
  // ==========================================================================
  
  /**
   * Handle key down events (shortcuts)
   */
  handleKeyDown(e) {
    // Disable all shortcuts if modal is open
    if (this.modalOpen) return;
    
    // Arrow keys: navigate images
    if (e.code === 'ArrowLeft') {
      e.preventDefault();
      Shiny.setInputValue('prev_image', { timestamp: Date.now() });
      return;
    }
    if (e.code === 'ArrowRight') {
      e.preventDefault();
      Shiny.setInputValue('next_image', { timestamp: Date.now() });
      return;
    }
    
    // Backspace: undo
    if (e.code === 'Backspace') {
      e.preventDefault();
      this.undo();
      return;
    }
    
    // "0" key: reset view
    if (e.code === 'Digit0' && !e.ctrlKey && !e.altKey && !e.shiftKey) {
      e.preventDefault();
      this.logAction('Reset view (zoom & pan)');
      this.resetToInitialView();
      return;
    }
    
    // Space: enable pan mode
    if (e.code === 'Space' && !this.editingPoint) {
      e.preventDefault();
      this.isPanning = true;
      this.spaceDown = true;
      return;
    }
    
    // Mode shortcuts: L, P, G, C, O
    const keyMap = {
      'KeyL': 'mode_landmark',
      'KeyP': 'mode_polyline',
      'KeyG': 'mode_polygon',
      'KeyC': 'mode_curve_open',
      'KeyO': 'mode_curve_closed'
    };
    
    if (e.code in keyMap) {
      document.getElementById(keyMap[e.code]).click();
    } 
    // Enter: create new partition
    else if (e.code === 'Enter') {
      const partitions = this.digitizedData[this.mode];
      const lastPartId = partitions.length;
      this.logAction(`Finished partition ${this.getModeLabel(this.mode).toLowerCase()} ${lastPartId} • starting new`);
      Shiny.setInputValue('new_partition', { timestamp: Date.now() });
    }
  }
  
  /**
   * Handle key up events
   */
  handleKeyUp(e) {
    if (e.code === 'Space') {
      this.isPanning = false;
      this.spaceDown = false;
    }
  }
  
  /**
   * Handle mouse wheel zoom
   */
  handleZoom(e) {
    e.preventDefault();
    
    const rect = this.canvas.getBoundingClientRect();
    const canvasX = e.clientX - rect.left;
    const canvasY = e.clientY - rect.top;
    
    // Get image coordinates before zoom
    const imgX = (canvasX - this.panX) / this.zoom;
    const imgY = (canvasY - this.panY) / this.zoom;
    
    // Apply zoom
    const zoomSpeed = 0.02;
    const delta = e.deltaY > 0 ? 1 - zoomSpeed : 1 + zoomSpeed;
    const newZoom = Math.max(0.3, Math.min(this.zoom * delta, 5));
    
    // Adjust pan to keep point under cursor fixed
    this.panX = canvasX - imgX * newZoom;
    this.panY = canvasY - imgY * newZoom;
    this.zoom = newZoom;
    
    this.redraw();
  }
  
  // ==========================================================================
  // MODE AND DATA MANAGEMENT
  // ==========================================================================
  
  /**
   * Set digitization mode
   */
  setMode(mode) {
    this.mode = mode;
    this.redraw();
  }
  
  /**
   * Set digitized data from external source
   */
  setDigitizedData(data, skipHistory = false) {
    this.digitizedData = data;
    if (!skipHistory) {
      this.saveToHistory(data);
    }
    this.redraw();
  }
  
  /**
   * Save current state to history
   */
  saveToHistory(data) {
    // Truncate history after current index
    this.history = this.history.slice(0, this.historyIndex + 1);
    
    // Add new state
    const cloned = JSON.parse(JSON.stringify(data));
    this.history.push(cloned);
    
    // Maintain max history size
    if (this.history.length > this.maxHistorySize) {
      this.history.shift();
    } else {
      this.historyIndex++;
    }
  }
  
  /**
   * Undo last action
   */
  undo() {
    if (this.historyIndex > 0) {
      this.historyIndex--;
      const previousState = JSON.parse(JSON.stringify(this.history[this.historyIndex]));
      this.digitizedData = this.cleanDigitizedData(previousState);
      this.redraw();
      this.logAction('Undo • restored previous state');
      
      // Sync cleaned state back to Shiny
      Shiny.setInputValue('canvas_undo', {
        digitized: this.digitizedData,
        timestamp: Date.now()
      });
    }
  }
  
  /**
   * Remove empty partitions from data (prevents ghost points)
   */
  cleanDigitizedData(data) {
    const cleaned = {
      landmarks: data.landmarks.filter(p => p.coords && p.coords.length > 0),
      polylines_open: data.polylines_open.filter(p => p.coords && p.coords.length > 0),
      polylines_closed: data.polylines_closed.filter(p => p.coords && p.coords.length > 0),
      curves_open: data.curves_open.filter(p => p.coords && p.coords.length > 0),
      curves_closed: data.curves_closed.filter(p => p.coords && p.coords.length > 0)
    };
    return cleaned;
  }
  
  // ==========================================================================
  // VIEW STATE MANAGEMENT
  // ==========================================================================
  
  /**
   * Store initial view state (for reset)
   */
  storeInitialViewState() {
    this.zoomInitial = this.zoom;
    this.panXInitial = this.panX;
    this.panYInitial = this.panY;
    Shiny.setInputValue('store_view_state', {
      zoom: this.zoomInitial,
      panX: this.panXInitial,
      panY: this.panYInitial
    });
  }
  
  /**
   * Reset view to initial state
   */
  resetToInitialView() {
    this.zoom = this.zoomInitial;
    this.panX = this.panXInitial;
    this.panY = this.panYInitial;
    this.redraw();
  }
  
  // ==========================================================================
  // IMAGE LOADING
  // ==========================================================================
  
  /**
   * Load image from base64 data
   */
  loadImage(base64Data) {
    if (!base64Data) {
      this.image = null;
      this.redraw();
      return;
    }
    
    const img = new Image();
    img.onload = () => {
      this.image = img;
      
      // Auto-fit image to canvas
      const scaleX = this.canvas.width / img.width;
      const scaleY = this.canvas.height / img.height;
      const scale = Math.min(scaleX, scaleY) * 0.95;
      
      this.zoom = scale;
      this.panX = (this.canvas.width - img.width * this.zoom) / 2;
      this.panY = (this.canvas.height - img.height * this.zoom) / 2;
      
      this.storeInitialViewState();
      this.redraw();
    };
    img.onerror = (e) => console.error('Failed to load image:', e);
    img.src = base64Data;
  }
  
  // ==========================================================================
  // HIT TESTING
  // ==========================================================================
  
  /**
   * Get point at canvas coordinates
   */
  getPointAtCanvas(canvasX, canvasY, tolerance = 8) {
    const tolerance_px = tolerance;
    const dataTypes = ['landmarks', 'polylines_open', 'polylines_closed', 'curves_open', 'curves_closed'];
    
    for (const dataType of dataTypes) {
      const partitions = this.digitizedData[dataType];
      for (let pid = 0; pid < partitions.length; pid++) {
        const partition = partitions[pid];
        for (let ptid = 0; ptid < partition.coords.length; ptid++) {
          const pt = partition.coords[ptid];
          const canvasPt = this.imageToCanvas(pt.x, pt.y);
          const dist = Math.hypot(canvasPt.x - canvasX, canvasPt.y - canvasY);
          
          if (dist < tolerance_px) {
            return {
              data_type: dataType,
              partition_id: pid + 1,
              point_idx: ptid + 1
            };
          }
        }
      }
    }
    
    return null;
  }
  
  /**
   * Get segment at canvas coordinates (for point insertion)
   */
  getSegmentAtCanvas(canvasX, canvasY, tolerance = 8) {
    const tolerance_px = tolerance;
    const exclusion_radius = 12;  // Don't insert near existing points
    const dataTypes = ['polylines_open', 'polylines_closed', 'curves_open', 'curves_closed'];
    
    for (const dataType of dataTypes) {
      const partitions = this.digitizedData[dataType];
      for (let pid = 0; pid < partitions.length; pid++) {
        const partition = partitions[pid];
        const coords = partition.coords;
        
        for (let i = 0; i < coords.length - 1; i++) {
          const p1 = coords[i];
          const p2 = coords[i + 1];
          const canvasP1 = this.imageToCanvas(p1.x, p1.y);
          const canvasP2 = this.imageToCanvas(p2.x, p2.y);
          
          const dist = this.distanceToSegment(canvasX, canvasY, canvasP1.x, canvasP1.y, canvasP2.x, canvasP2.y);
          
          if (dist.distance < tolerance_px) {
            const distToP1 = Math.hypot(dist.closestX - canvasP1.x, dist.closestY - canvasP1.y);
            const distToP2 = Math.hypot(dist.closestX - canvasP2.x, dist.closestY - canvasP2.y);
            
            // Only insert if not too close to existing points
            if (distToP1 > exclusion_radius && distToP2 > exclusion_radius) {
              const imgPoint = this.canvasToImage(dist.closestX, dist.closestY);
              return {
                data_type: dataType,
                partition_id: pid + 1,
                insert_idx: i + 2,
                x: imgPoint.x,
                y: imgPoint.y
              };
            }
          }
        }
      }
    }
    
    return null;
  }
  
  /**
   * Calculate distance from point to line segment
   */
  distanceToSegment(px, py, x1, y1, x2, y2) {
    const A = px - x1;
    const B = py - y1;
    const C = x2 - x1;
    const D = y2 - y1;
    
    const dot = A * C + B * D;
    const lenSq = C * C + D * D;
    let param = lenSq !== 0 ? dot / lenSq : -1;
    
    let xx, yy;
    if (param < 0) {
      xx = x1;
      yy = y1;
    } else if (param > 1) {
      xx = x2;
      yy = y2;
    } else {
      xx = x1 + param * C;
      yy = y1 + param * D;
    }
    
    const dx = px - xx;
    const dy = py - yy;
    return {
      distance: Math.sqrt(dx * dx + dy * dy),
      closestX: xx,
      closestY: yy
    };
  }
  
  // ==========================================================================
  // LABEL GENERATION
  // ==========================================================================
  
  /**
   * Get human-readable mode label
   */
  getModeLabel(mode) {
    const labels = {
      'landmarks': 'Landmarks',
      'polylines_open': 'Polylines',
      'polylines_closed': 'Polygons',
      'curves_open': 'Curves (open)',
      'curves_closed': 'Curves (closed)'
    };
    return labels[mode] || mode;
  }
  
  /**
   * Get point label (e.g., "L1.3")
   */
  getPointLabel(pointHit) {
    const typeMap = {
      'landmarks': 'L',
      'polylines_open': 'P',
      'polylines_closed': 'G',
      'curves_open': 'C',
      'curves_closed': 'O'
    };
    const prefix = typeMap[pointHit.data_type] || 'X';
    return `${prefix}${pointHit.partition_id}.${pointHit.point_idx}`;
  }
  
  /**
   * Get label for last added point
   */
  getLastPointLabel() {
    const typeMap = {
      'landmarks': 'L',
      'polylines_open': 'P',
      'polylines_closed': 'G',
      'curves_open': 'C',
      'curves_closed': 'O'
    };
    
    const partitions = this.digitizedData[this.mode];
    if (!partitions || partitions.length === 0) return null;
    
    const lastPartId = partitions.length;
    const lastPartition = partitions[lastPartId - 1];
    if (!lastPartition.coords || lastPartition.coords.length === 0) return null;
    
    const lastPtId = lastPartition.coords.length;
    const prefix = typeMap[this.mode] || 'X';
    return {
      label: `${prefix}${lastPartId}.${lastPtId}`,
      x: Math.round(lastPartition.coords[lastPtId - 1].x),
      y: Math.round(lastPartition.coords[lastPtId - 1].y),
      nextLabel: `${prefix}${lastPartId}.${lastPtId + 1}`
    };
  }
  
  // ==========================================================================
  // RENDERING
  // ==========================================================================
  
  /**
   * Redraw entire canvas
   */
  redraw() {
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    
    // Apply transformations
    this.ctx.save();
    this.ctx.translate(this.panX, this.panY);
    this.ctx.scale(this.zoom, this.zoom);
    
    // Draw image
    if (this.image) {
      this.ctx.drawImage(this.image, 0, 0);
    } else {
      this.ctx.fillStyle = '#333';
      this.ctx.fillRect(0, 0, 10000, 10000);
    }
    
    // Draw all data types (order matters for layering)
    this.digitizedData.polylines_closed.forEach((partition) => {
      this.drawPolyline(partition.coords, true, 'polylines_closed');
    });
    
    this.digitizedData.polylines_open.forEach((partition) => {
      this.drawPolyline(partition.coords, false, 'polylines_open');
    });
    
    this.digitizedData.curves_closed.forEach((partition) => {
      const curvePoints = this.generateCurvePoints(partition.coords, true);
      this.drawCurve(curvePoints, true, 'curves_closed');
    });
    
    this.digitizedData.curves_open.forEach((partition) => {
      const curvePoints = this.generateCurvePoints(partition.coords, false);
      this.drawCurve(curvePoints, false, 'curves_open');
    });
    
    // Draw control points on top
    this.drawPointsForType('landmarks');
    this.drawPointsForType('polylines_open');
    this.drawPointsForType('polylines_closed');
    this.drawPointsForType('curves_open');
    this.drawPointsForType('curves_closed');
    
    this.ctx.restore();
    
    // Draw segment preview (not transformed)
    this.drawSegmentPreview();
  }
  
  /**
   * Draw all points for a data type
   */
  drawPointsForType(dataType) {
    const typeMap = { 
      'landmarks': 'l', 
      'polylines_open': 'p', 
      'polylines_closed': 'g', 
      'curves_open': 'c', 
      'curves_closed': 'o' 
    };
    const prefix = typeMap[dataType] || 'x';
    
    this.digitizedData[dataType].forEach((partition, pid) => {
      partition.coords.forEach((pt, ptid) => {
        const label = `${prefix}${pid + 1}.${ptid + 1}`;
        const isCorner = pt.is_corner || false;
        this.drawPoint(pt.x, pt.y, label, dataType, pid + 1, ptid + 1, isCorner);
      });
    });
  }
  
  /**
   * Draw segment insertion preview
   */
  drawSegmentPreview() {
    if (this.hoverSegment && !this.isDragging) {
      const previewPt = this.imageToCanvas(this.hoverSegment.x, this.hoverSegment.y);
      
      const dataType = this.hoverSegment.data_type;
      const partitions = this.digitizedData[dataType];
      const partition = partitions[this.hoverSegment.partition_id - 1];
      const idx = this.hoverSegment.insert_idx - 2;
      
      if (partition && idx >= 0 && idx < partition.coords.length - 1) {
        const p1 = this.imageToCanvas(partition.coords[idx].x, partition.coords[idx].y);
        const p2 = this.imageToCanvas(partition.coords[idx + 1].x, partition.coords[idx + 1].y);
        
        // Draw preview lines
        this.ctx.strokeStyle = 'rgba(100, 200, 255, 0.4)';
        this.ctx.lineWidth = 1;
        this.ctx.beginPath();
        this.ctx.moveTo(p1.x, p1.y);
        this.ctx.lineTo(previewPt.x, previewPt.y);
        this.ctx.lineTo(p2.x, p2.y);
        this.ctx.stroke();
      }
      
      // Draw preview point
      this.ctx.fillStyle = 'rgba(100, 200, 255, 0.7)';
      this.ctx.beginPath();
      this.ctx.arc(previewPt.x, previewPt.y, 4, 0, Math.PI * 2);
      this.ctx.fill();
      
      this.ctx.strokeStyle = 'rgba(100, 200, 255, 0.9)';
      this.ctx.lineWidth = 1.5;
      this.ctx.stroke();
    }
  }
  
  /**
   * Draw a single point
   */
  drawPoint(x, y, label, data_type, partition_id, point_idx, isCorner) {
    const radius = 2.5 / this.zoom;
    
    const isHover = this.hoverPoint && 
                    this.hoverPoint.data_type === data_type &&
                    this.hoverPoint.partition_id === partition_id &&
                    this.hoverPoint.point_idx === point_idx;
    
    // Color by type
    let fillColor = '#FF5733';
    if (data_type === 'polylines_open') fillColor = '#3357FF';
    if (data_type === 'polylines_closed') fillColor = '#33FF57';
    if (data_type === 'curves_open') fillColor = '#FF1493';
    if (data_type === 'curves_closed') fillColor = '#FFD700';
    
    if (isCorner) fillColor = '#FFA500';
    if (isHover) fillColor = '#FFFFFF';
    
    this.canvas.style.cursor = isHover ? 'grab' : 'crosshair';
    this.ctx.fillStyle = fillColor;
    
    const size = isHover ? radius * 1.5 : radius;
    
    // Draw point (square if corner, circle otherwise)
    if (isCorner) {
      this.ctx.fillRect(x - size, y - size, size * 2, size * 2);
    } else {
      this.ctx.beginPath();
      this.ctx.arc(x, y, size, 0, Math.PI * 2);
      this.ctx.fill();
    }
    
    // Draw outline
    this.ctx.strokeStyle = isHover ? '#000' : '#fff';
    this.ctx.lineWidth = isHover ? 2 / this.zoom : 1 / this.zoom;
    
    if (isCorner) {
      this.ctx.strokeRect(x - size, y - size, size * 2, size * 2);
    } else {
      this.ctx.beginPath();
      this.ctx.arc(x, y, size, 0, Math.PI * 2);
      this.ctx.stroke();
    }
    
    // Draw label
    this.ctx.fillStyle = '#fff';
    this.ctx.font = `${10 / this.zoom}px monospace`;
    this.ctx.textAlign = 'left';
    this.ctx.textBaseline = 'top';
    
    const labelX = x + (radius * 2);
    const labelY = y - (radius * 1.5);
    const textMetrics = this.ctx.measureText(label);
    const bgPadding = 2 / this.zoom;
    
    // Label background
    this.ctx.fillStyle = 'rgba(0, 0, 0, 0.6)';
    this.ctx.fillRect(
      labelX - bgPadding, 
      labelY - bgPadding, 
      textMetrics.width + bgPadding * 2, 
      (10 / this.zoom) + bgPadding * 2
    );
    
    // Label text
    this.ctx.fillStyle = '#fff';
    this.ctx.fillText(label, labelX, labelY);
  }
  
  /**
   * Draw polyline or polygon
   */
  drawPolyline(coords, isClosed, data_type) {
    if (!coords || coords.length === 0) return;
    
    const color = data_type === 'polylines_closed' ? '#33FF57' : '#3357FF';
    
    this.ctx.strokeStyle = color;
    this.ctx.lineWidth = 1.5 / this.zoom;
    this.ctx.beginPath();
    this.ctx.moveTo(coords[0].x, coords[0].y);
    
    for (let i = 1; i < coords.length; i++) {
      this.ctx.lineTo(coords[i].x, coords[i].y);
    }
    
    if (isClosed && coords.length > 1) {
      this.ctx.lineTo(coords[0].x, coords[0].y);
    }
    
    this.ctx.stroke();
    
    // Fill if closed
    if (isClosed && coords.length > 2) {
      this.ctx.fillStyle = color + '20';
      this.ctx.fill();
    }
  }
  
  /**
   * Draw interpolated curve
   */
  drawCurve(curvePoints, isClosed, data_type) {
    if (!curvePoints || curvePoints.length === 0) return;
    
    const color = data_type === 'curves_closed' ? '#FFD700' : '#FF1493';
    
    this.ctx.strokeStyle = color;
    this.ctx.lineWidth = 1.5 / this.zoom;
    this.ctx.beginPath();
    this.ctx.moveTo(curvePoints[0].x, curvePoints[0].y);
    
    for (let i = 1; i < curvePoints.length; i++) {
      this.ctx.lineTo(curvePoints[i].x, curvePoints[i].y);
    }
    
    if (isClosed && curvePoints.length > 1) {
      this.ctx.lineTo(curvePoints[0].x, curvePoints[0].y);
    }
    
    this.ctx.stroke();
    
    // Fill if closed
    if (isClosed && curvePoints.length > 2) {
      this.ctx.fillStyle = color + '20';
      this.ctx.fill();
    }
  }
}

// ============================================================================
// GLOBAL INSTANCE
// ============================================================================

let digitizer = null;

/**
 * Initialize digitizer when DOM is ready
 */
document.addEventListener('DOMContentLoaded', () => {
  digitizer = new DigitizeCanvas('canvas');
});

// ============================================================================
// SHINY MESSAGE HANDLERS
// ============================================================================

/**
 * Load new image and digitized data
 */
Shiny.addCustomMessageHandler('load_image', function(data) {
  if (!digitizer) {
    const canvas = document.getElementById('canvas');
    if (canvas) {
      digitizer = new DigitizeCanvas('canvas');
    } else {
      console.error('Canvas element not found!');
      return;
    }
  }
  
  digitizer.loadImage(data.image);
  digitizer.setDigitizedData(data.digitized);
});

/**
 * Update digitized data (with history)
 */
Shiny.addCustomMessageHandler('update_digitized', function(data) {
  if (digitizer) {
    digitizer.setDigitizedData(data, false);
  }
});

/**
 * Update digitized data (without adding to history)
 */
Shiny.addCustomMessageHandler('update_digitized_no_history', function(data) {
  if (digitizer) {
    digitizer.setDigitizedData(data, true);
  }
});

/**
 * Reset view to initial state
 */
Shiny.addCustomMessageHandler('reset_view', function(data) {
  if (digitizer) {
    digitizer.storeInitialViewState();
  }
});

/**
 * Set digitization mode
 */
Shiny.addCustomMessageHandler('set_mode', function(mode) {
  if (digitizer) {
    digitizer.setMode(mode);
    const modeLabels = {
      'landmarks': 'Landmarks',
      'polylines_open': 'Polylines',
      'polylines_closed': 'Polygons',
      'curves_open': 'Curves (open)',
      'curves_closed': 'Curves (closed)'
    };
    digitizer.logAction(`Switched to ${modeLabels[mode]} mode`);
  }
  
  // Update UI button states
  const modeMap = {
    'landmarks': 'mode_landmark',
    'polylines_open': 'mode_polyline',
    'polylines_closed': 'mode_polygon',
    'curves_open': 'mode_curve_open',
    'curves_closed': 'mode_curve_closed'
  };
  
  const buttons = document.querySelectorAll('[id^="mode_"]');
  buttons.forEach(btn => btn.classList.remove('active'));
  
  const activeBtn = document.getElementById(modeMap[mode]);
  if (activeBtn) activeBtn.classList.add('active');
});

/**
 * Update partition count badges
 */
Shiny.addCustomMessageHandler('update_partition_counts', function(counts) {
  const modes = ['landmarks', 'polylines_open', 'polylines_closed', 'curves_open', 'curves_closed'];
  const modeMap = {
    'landmarks': 'mode_landmark',
    'polylines_open': 'mode_polyline',
    'polylines_closed': 'mode_polygon',
    'curves_open': 'mode_curve_open',
    'curves_closed': 'mode_curve_closed'
  };
  
  modes.forEach(mode => {
    const count = counts[mode] || 0;
    const btn = document.getElementById(modeMap[mode]);
    if (btn) {
      let countEl = btn.querySelector('.partition-count');
      if (!countEl && count > 0) {
        countEl = document.createElement('span');
        countEl.className = 'partition-count';
        btn.appendChild(countEl);
      }
      if (countEl) {
        countEl.textContent = count;
        countEl.style.display = count > 0 ? 'block' : 'none';
      }
    }
  });
});

/**
 * Setup double-click handlers for mode buttons (future feature)
 */
Shiny.addCustomMessageHandler('setup_partition_clicks', function(counts) {
  const modes = ['landmarks', 'polylines_open', 'polylines_closed', 'curves_open', 'curves_closed'];
  const modeMap = {
    'landmarks': 'mode_landmark',
    'polylines_open': 'mode_polyline',
    'polylines_closed': 'mode_polygon',
    'curves_open': 'mode_curve_open',
    'curves_closed': 'mode_curve_closed'
  };
  
  modes.forEach(mode => {
    const btn = document.getElementById(modeMap[mode]);
    if (btn) {
      btn.removeEventListener('dblclick', btn._dblClickHandler);
      btn._dblClickHandler = (e) => {
        e.stopPropagation();
        Shiny.setInputValue('delete_all_partitions_of_mode', {
          mode: mode,
          timestamp: Date.now()
        });
      };
      btn.addEventListener('dblclick', btn._dblClickHandler);
    }
  });
});

/**
 * Set filter mode and update UI
 */
Shiny.addCustomMessageHandler('set_filter_mode', function(mode) {
  const filterBtnMap = {
    'all': 'filter_all',
    'done': 'filter_done',
    'undone': 'filter_undone'
  };
  
  ['filter_all', 'filter_done', 'filter_undone'].forEach(id => {
    const btn = document.getElementById(id);
    if (btn) btn.classList.remove('active');
  });
  
  const activeBtn = document.getElementById(filterBtnMap[mode]);
  if (activeBtn) activeBtn.classList.add('active');
});

/**
 * Update filter count badges
 */
Shiny.addCustomMessageHandler('update_filter_counts', function(counts) {
  const filterData = [
    { id: 'filter_all', count: counts.all },
    { id: 'filter_done', count: counts.done },
    { id: 'filter_undone', count: counts.undone }
  ];
  
  filterData.forEach(item => {
    const btn = document.getElementById(item.id);
    if (btn) {
      let countEl = btn.querySelector('.partition-count');
      if (!countEl && item.count > 0) {
        countEl = document.createElement('span');
        countEl.className = 'partition-count';
        btn.appendChild(countEl);
      }
      if (countEl) {
        countEl.textContent = item.count;
        countEl.style.display = item.count > 0 ? 'block' : 'none';
      }
    }
  });
});