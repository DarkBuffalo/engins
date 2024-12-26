;;; engins.el --- Gestion des engins de chantier


;;; Commentary:
;;
;; Gestion des engins de chantier
;;
;;; Code:

;; -*- lexical-binding: t -*-

(require 'sqlite)
(require 'calendar)
(require 'widget)

;; Définition de la base de données
(defvar engins-db (expand-file-name "engins.db" user-emacs-directory)
  "Chemin vers la base de données SQLite.")

(defun engins-init-db ()
  "Initialise la base de données avec les tables nécessaires."
  (let ((db (sqlite-open engins-db)))
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS engins (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        type TEXT NOT NULL,
        reference TEXT NOT NULL UNIQUE,
        prix_journalier REAL NOT NULL,
        disponible BOOLEAN DEFAULT 1
      )")
    
    (sqlite-execute db "
      CREATE TABLE IF NOT EXISTS locations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        engin_id INTEGER NOT NULL,
        client TEXT NOT NULL,
        date_debut DATE NOT NULL,
        date_fin DATE NOT NULL,
        prix_total REAL NOT NULL,
        statut TEXT DEFAULT 'en_cours',
        FOREIGN KEY (engin_id) REFERENCES engins(id)
      )")
    (sqlite-close db)))

;; Fonctions CRUD pour les engins
(defun engins-ajouter (type reference prix-journalier)
  "Ajoute un nouvel engin dans la base de données."
  (let ((db (sqlite-open engins-db)))
    (sqlite-execute db
                   "INSERT INTO engins (type, reference, prix_journalier) VALUES (?, ?, ?)"
                   (vector type reference prix-journalier))
    (sqlite-close db)))

(defun engins-liste ()
  "Retourne la liste de tous les engins."
  (let ((db (sqlite-open engins-db)))
    (let ((resultat (sqlite-select db "SELECT * FROM engins")))
      (sqlite-close db)
      resultat)))

;; Fonctions pour la gestion des locations
(defun engins-creer-location (engin-id client date-debut date-fin)
  "Crée une nouvelle location."
  (let* ((db (sqlite-open engins-db))
         (prix-journalier (caar (sqlite-select db
                                             "SELECT prix_journalier FROM engins WHERE id = ?"
                                             (vector engin-id))))
         (jours (float (- (time-to-days date-fin) (time-to-days date-debut))))
         (prix-total (* prix-journalier jours)))
    (sqlite-execute db
                   "INSERT INTO locations (engin_id, client, date_debut, date_fin, prix_total)
                    VALUES (?, ?, ?, ?, ?)"
                   (vector engin-id client date-debut date-fin prix-total))
    (sqlite-execute db
                   "UPDATE engins SET disponible = 0 WHERE id = ?"
                   (vector engin-id))
    (sqlite-close db)))

(defun engins-terminer-location (location-id)
  "Termine une location et rend l'engin disponible."
  (let ((db (sqlite-open engins-db)))
    (sqlite-execute db
                   "UPDATE locations SET statut = 'terminé' WHERE id = ?"
                   (vector location-id))
    (sqlite-execute db
                   "UPDATE engins SET disponible = 1 
                    WHERE id = (SELECT engin_id FROM locations WHERE id = ?)"
                   (vector location-id))
    (sqlite-close db)))

;; Interface utilisateur
;;;###autoload
(defun engins-ui ()
  "Affiche l'interface principale de gestion des locations."
  (interactive)
  (switch-to-buffer "*Engins*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (widget-insert "\n=== Gestion des Locations d'Engins ===\n\n")
  
  ;; Menu principal
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-ajouter-engin))
                 "Ajouter un engin")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-liste-engins))
                 "Liste des engins")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-nouvelle-location))
                 "Nouvelle location")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-liste-locations))
                 "Liste des locations")
  
  (use-local-map widget-keymap)
  (widget-setup))

(defun engins-ui-ajouter-engin ()
  "Interface pour ajouter un nouvel engin."
  (let ((type (read-string "Type d'engin: "))
        (reference (read-string "Référence: "))
        (prix (read-number "Prix journalier: ")))
    (engins-ajouter type reference prix)
    (message "Engin ajouté avec succès !")))

(defun engins-ui-liste-engins ()
  "Affiche la liste des engins."
  (switch-to-buffer "*Liste Engins*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (dolist (engin (engins-liste))
    (insert (format "ID: %d | Type: %s | Réf: %s | Prix: %.2f€ | %s\n"
                   (nth 0 engin)
                   (nth 1 engin)
                   (nth 2 engin)
                   (nth 3 engin)
                   (if (= (nth 4 engin) 1) "Disponible" "En location"))))
  (local-set-key (kbd "q") 'kill-buffer))

;; Initialisation
;;;###autoload
(defun engins-setup ()
  "Configure l'application de gestion des engins."
  (interactive)
  (engins-init-db)
  (engins-ui))

(provide 'engins)
;;; engins.el ends here
