;;; engins.el --- Gestion des engins de chantier
;; Copyright (C) 2024

;; Author: Votre Nom <votre@email.com>
;; Maintainer: Votre Nom <votre@email.com>
;; Created: 26 Dec 2024
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (sqlite "0.1"))
;; Keywords: tools
;; URL: https://github.com/votre-compte/engins

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Ce package fournit une interface pour la gestion des engins de chantier.
;; Il permet de :
;; - Gérer plusieurs chantiers
;; - Ajouter et suivre les engins par chantier
;; - Gérer les locations d'engins
;; - Suivre la disponibilité des engins

;;; Code:


(require 'sqlite)
(require 'calendar)
(require 'widget)

;; Fonction de réinitialisation de la base
(defun engins-reset-db ()
  "Supprime et réinitialise la base de données."
  (interactive)
  (when (file-exists-p engins-db)
    (delete-file engins-db))
  (engins-init-db))

;; Définition de la base de données
(defvar engins-db (expand-file-name "engins.db" user-emacs-directory)
  "Chemin vers la base de données SQLite.")

(defun engins-init-db ()
  "Initialise la base de données avec les tables nécessaires."
  (let ((db (sqlite-open engins-db)))
    ;; Table des chantiers d'abord (car référencée par les autres)
    (sqlite-execute db "DROP TABLE IF EXISTS locations")
    (sqlite-execute db "DROP TABLE IF EXISTS engins")
    (sqlite-execute db "DROP TABLE IF EXISTS chantiers")
    
    (sqlite-execute db "
      CREATE TABLE chantiers (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        nom TEXT NOT NULL,
        adresse TEXT NOT NULL,
        date_debut DATE NOT NULL,
        date_fin DATE,
        statut TEXT DEFAULT 'en_cours'
      )")
    
    (sqlite-execute db "
      CREATE TABLE engins (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        type TEXT NOT NULL,
        reference TEXT NOT NULL UNIQUE,
        prix_journalier REAL NOT NULL,
        chantier_id INTEGER,
        disponible BOOLEAN DEFAULT 1,
        FOREIGN KEY (chantier_id) REFERENCES chantiers(id)
      )")
    
    (sqlite-execute db "
      CREATE TABLE locations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        engin_id INTEGER NOT NULL,
        chantier_id INTEGER NOT NULL,
        client TEXT NOT NULL,
        date_debut DATE NOT NULL,
        date_fin DATE NOT NULL,
        prix_total REAL NOT NULL,
        statut TEXT DEFAULT 'en_cours',
        FOREIGN KEY (engin_id) REFERENCES engins(id),
        FOREIGN KEY (chantier_id) REFERENCES chantiers(id)
      )")
    (sqlite-close db)))

;; Fonctions CRUD pour les chantiers
(defun engins-ajouter-chantier (nom adresse date-debut)
  "Ajoute un nouveau chantier."
  (let ((db (sqlite-open engins-db)))
    (sqlite-execute db
                   "INSERT INTO chantiers (nom, adresse, date_debut) VALUES (?, ?, ?)"
                   (vector nom adresse date-debut))
    (sqlite-close db)))

(defun engins-liste-chantiers ()
  "Retourne la liste de tous les chantiers actifs."
  (let ((db (sqlite-open engins-db)))
    (let ((resultat (sqlite-select db "SELECT * FROM chantiers WHERE statut = 'en_cours'")))
      (sqlite-close db)
      resultat)))

;; Fonctions CRUD pour les engins
(defun engins-ajouter (type reference prix-journalier chantier-id)
  "Ajoute un nouvel engin et l'assigne à un chantier."
  (let ((db (sqlite-open engins-db)))
    (sqlite-execute db
                   "INSERT INTO engins (type, reference, prix_journalier, chantier_id) 
                    VALUES (?, ?, ?, ?)"
                   (vector type reference prix-journalier chantier-id))
    (sqlite-close db)))

(defun engins-liste-par-chantier (chantier-id)
  "Retourne la liste des engins d'un chantier spécifique."
  (let ((db (sqlite-open engins-db)))
    (let ((resultat (sqlite-select db 
                                 "SELECT e.*, c.nom as chantier_nom 
                                  FROM engins e 
                                  LEFT JOIN chantiers c ON e.chantier_id = c.id 
                                  WHERE e.chantier_id = ?"
                                 (vector chantier-id))))
      (sqlite-close db)
      resultat)))

(defun engins-deplacer (engin-id nouveau-chantier-id)
  "Déplace un engin vers un nouveau chantier."
  (let ((db (sqlite-open engins-db)))
    (sqlite-execute db
                   "UPDATE engins SET chantier_id = ? WHERE id = ?"
                   (vector nouveau-chantier-id engin-id))
    (sqlite-close db)))

;; Interface utilisateur
;;;###autoload
(defun engins-ui ()
  "Affiche l'interface principale de gestion des engins."
  (interactive)
  (switch-to-buffer "*Engins*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (widget-insert "\n=== Gestion des Engins par Chantier ===\n\n")
  
  ;; Menu principal
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-ajouter-chantier))
                 "Nouveau Chantier")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-liste-chantiers))
                 "Liste des Chantiers")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (engins-ui-ajouter-engin))
                 "Ajouter un Engin")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                          (call-interactively 'engins-ui-liste-engins-chantier))
                 "Engins par Chantier")
  
  (use-local-map widget-keymap)
  (widget-setup))

(defun engins-ui-ajouter-chantier ()
  "Interface pour ajouter un nouveau chantier."
  (let ((nom (read-string "Nom du chantier: "))
        (adresse (read-string "Adresse: "))
        (date-debut (format-time-string "%Y-%m-%d")))
    (engins-ajouter-chantier nom adresse date-debut)
    (message "Chantier ajouté avec succès !")))

(defun engins-ui-liste-chantiers ()
  "Affiche la liste des chantiers."
  (switch-to-buffer "*Liste Chantiers*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (dolist (chantier (engins-liste-chantiers))
    (insert (format "ID: %d | Nom: %s | Adresse: %s | Début: %s | Statut: %s\n"
                   (nth 0 chantier)
                   (nth 1 chantier)
                   (nth 2 chantier)
                   (nth 3 chantier)
                   (nth 5 chantier))))
  (local-set-key (kbd "q") 'kill-buffer))

(defun engins-ui-ajouter-engin ()
  "Interface pour ajouter un nouvel engin."
  (let* ((chantiers (engins-liste-chantiers))
         (chantier-names (mapcar (lambda (c) (format "%d: %s" (nth 0 c) (nth 1 c))) chantiers))
         (chantier-choice (completing-read "Chantier: " chantier-names))
         (chantier-id (string-to-number (car (split-string chantier-choice ":"))))
         (type (read-string "Type d'engin: "))
         (reference (read-string "Référence: "))
         (prix (read-number "Prix journalier: ")))
    (engins-ajouter type reference prix chantier-id)
    (message "Engin ajouté avec succès !")))

(defun engins-ui-liste-engins-chantier (chantier-id)
  "Affiche la liste des engins pour un chantier spécifique."
  (interactive "nID du chantier: ")
  (switch-to-buffer "*Liste Engins par Chantier*")
  (let ((inhibit-read-only t))
    (erase-buffer))
  (dolist (engin (engins-liste-par-chantier chantier-id))
    (insert (format "ID: %d | Type: %s | Réf: %s | Prix: %.2f€ | Chantier: %s\n"
                   (nth 0 engin)
                   (nth 1 engin)
                   (nth 2 engin)
                   (nth 3 engin)
                   (nth 6 engin))))
  (local-set-key (kbd "q") 'kill-buffer))

;;;###autoload
(defun engins-setup ()
  "Configure l'application de gestion des engins."
  (interactive)
  (engins-init-db)
  (engins-ui))


;;; engins.el ends here
