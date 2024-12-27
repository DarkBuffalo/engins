;;; engins.el --- Gestion des engins de chantier
;; Copyright (C) 2024

;; Author: DarkBuffalo <db@gnu.re>
;; Maintainer: DarkBuffalo <db@gnu.re>
;; Created: 26 Dec 2024
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (sqlite "0.1"))
;; Keywords: tools
;; URL: https://github.com/DarkBuffalo/engins

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
(require 'tabulated-list)



;; Définition de la base de données
(defvar engins-db (expand-file-name "engins.db" user-emacs-directory)
  "Chemin vers la base de données SQLite.")

;; Fonction de réinitialisation de la base
(defun engins-reset-db ()
  "Supprime et réinitialise la base de données."
  (interactive)
  (when (file-exists-p engins-db)
    (delete-file engins-db))
  (engins-init-db))

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
        FOREIGN KEY (chantier_id) REFERENCES chantiers(id)
      )")
    
    (sqlite-execute db "
      CREATE TABLE locations (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        engin_id INTEGER NOT NULL,
        chantier_id INTEGER NOT NULL,
        agence TEXT NOT NULL,
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
  "Retourne la liste des engins associés à un chantier donné."
  (let ((db (sqlite-open engins-db)))
    (message "Base de données ouverte : %S" db)
    (message "Chantier ID utilisé : %S" chantier-id)
    (let ((resultat (sqlite-select db 
                                 "SELECT 
                                    e.id,
                                    e.type,
                                    e.reference,
                                    e.prix_journalier,
                                    e.chantier_id,
                                    c.nom as chantier_nom
                                  FROM engins e 
                                  LEFT JOIN chantiers c ON e.chantier_id = c.id 
                                  WHERE e.chantier_id = ?"
                                 (vector chantier-id))))
      (message "Résultat SQL : %S" resultat)
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
                           (engins-ui-ajouter-location))
                 "Ajouter Location")
  (widget-insert "  ")
  (widget-create 'push-button
                 :notify (lambda (&rest _ignore)
                           (engins-ui-liste-engins-chantier))
                 "Engins par Chantier")
  
  (use-local-map widget-keymap)
  (widget-setup))


(defun engins-ui-ajouter-chantier ()
  "Interface pour ajouter un nouveau chantier."
  (let ((nom (read-string "Nom du chantier: "))
        (adresse (read-string "Adresse: "))
        (date-debut (org-read-date nil nil nil "Date de démmarage ?")))
    (engins-ajouter-chantier nom adresse date-debut)
    (message "Chantier ajouté avec succès !")))



;;; Chantier via tabulated-list-mode
(defun engins-ui-liste-chantiers ()
  "Affiche la liste des chantiers en utilisant `tabulated-list-mode`."
  (interactive)
  (let ((buffer (get-buffer-create "*Liste Chantiers*")))
    (with-current-buffer buffer
      (engins-liste-chantiers-mode)
      (engins-liste-chantiers-refresh))
    (switch-to-buffer buffer)))

(define-derived-mode engins-liste-chantiers-mode tabulated-list-mode "Liste Chantiers"
  "Mode majeur pour lister les chantiers."
  (setq tabulated-list-format [("ID" 5 t)
                               ("Nom" 20 t)
                               ("Adresse" 30 t)
                               ("Début" 12 t)
                               ("Statut" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "ID" nil))
  (add-hook 'tabulated-list-revert-hook 'engins-liste-chantiers-refresh nil t)
  (tabulated-list-init-header))

(defun engins-liste-chantiers-refresh ()
  "Rafraîchit la liste des chantiers."
  (setq tabulated-list-entries (mapcar
                                (lambda (chantier)
                                  (let ((id (number-to-string (nth 0 chantier)))
                                        (nom (nth 1 chantier))
                                        (adresse (nth 2 chantier))
                                        (date-debut (nth 3 chantier))
                                        (statut (nth 5 chantier)))
                                    (list id (vector id nom adresse date-debut statut))))
                                (engins-liste-chantiers)))
  (tabulated-list-print t))

(add-hook 'engins-liste-chantiers-mode-hook 'hl-line-mode)



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


;;; Engins via tabulated-list-mode

(defun engins-get-chantier-id ()
  "Permet à l'utilisateur de sélectionner un chantier et retourne son ID."
  (let* ((chantiers (engins-liste-chantiers))
         (chantier-names (mapcar (lambda (c) 
                                  (format "%d: %s" (nth 0 c) (nth 1 c))) 
                                chantiers))
         (chantier-choice (completing-read "Sélectionnez un chantier: " 
                                         chantier-names nil t))
         (id-str (car (split-string chantier-choice ":")))
         (chantier-id (if (and id-str (not (string= id-str "")))
                         (string-to-number id-str)
                       nil)))
    (message "Chantiers disponibles: %S" chantier-names)
    (message "Choix sélectionné: %S" chantier-choice)
    (message "ID extrait: %S" chantier-id)
    chantier-id))


;; Définir la variable comme buffer-local par défaut
(defvar-local engins-current-chantier-id nil
  "ID du chantier actuellement affiché dans la liste des engins.")



(define-derived-mode engins-liste-engins-mode tabulated-list-mode "Liste Engins"
  "Mode majeur pour lister les engins d'un chantier."
  (setq tabulated-list-format [("ID" 5 t)
                              ("Type" 15 t)
                              ("Référence" 15 t)
                              ("Prix/Jour" 10 t)
                              ("Chantier" 20 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "ID" nil))
  (remove-hook 'tabulated-list-revert-hook 'engins-liste-engins-refresh t)
  (add-hook 'tabulated-list-revert-hook 'engins-liste-engins-refresh nil t)
  (tabulated-list-init-header))


(defun engins-ui-liste-engins-chantier ()
  "Affiche la liste des engins en utilisant `tabulated-list-mode`."
  (interactive)
  (let* ((chantier-id (engins-get-chantier-id))
         (buffer (get-buffer-create "*Liste Engins par Chantier*")))
    (with-current-buffer buffer
      (engins-liste-engins-mode)
      (setq engins-current-chantier-id chantier-id) ; Définir l'ID avant le refresh
      (tabulated-list-revert)
      (message "ID défini dans le buffer: %S" engins-current-chantier-id))
    (switch-to-buffer buffer)))


(defun engins-liste-engins-refresh ()
  "Rafraîchit la liste des engins."
  (let ((id engins-current-chantier-id))
    (message "Rafraîchissement avec ID: %S" id)
    (when id
      (let ((engins (engins-liste-par-chantier id)))
        (message "Engins trouvés: %S" engins)
        (setq tabulated-list-entries
              (mapcar (lambda (engin)
                       (let* ((id (or (nth 0 engin) ""))
                             (type (or (nth 1 engin) ""))
                             (reference (or (nth 2 engin) ""))
                             (prix-journalier (nth 3 engin))
                             (prix (if (numberp prix-journalier)
                                     (format "%.2f€" prix-journalier)
                                   "N/A"))
                             (chantier-nom (or (nth 5 engin) "Non assigné")))
                         (list (number-to-string id)
                               (vector 
                                (number-to-string id)
                                (if (stringp type) type "")
                                (if (stringp reference) reference "")
                                prix
                                (if (stringp chantier-nom) chantier-nom "Non assigné")))))
                     engins))
        (tabulated-list-print t)))))


(add-hook 'engins-liste-engins-mode-hook 'hl-line-mode)


;;; Gestion des locations dans engins.el

(defun engins-ajouter-location (engin-id chantier-id agence date-debut date-fin)
  "Ajoute une nouvelle location pour un engin."
  (let* ((db (sqlite-open engins-db))
         (prix-journalier (caar (sqlite-select db "SELECT prix_journalier FROM engins WHERE id = ?" (vector engin-id))))
         (date-debut-time (date-to-time date-debut))
         (date-fin-time (date-to-time date-fin))
         (jours (max 1 (- (time-to-days date-fin-time) (time-to-days date-debut-time))))
         (prix-total (* prix-journalier jours)))
    (sqlite-execute db
                    "INSERT INTO locations (engin_id, chantier_id, agence, date_debut, date_fin, prix_total) \
                     VALUES (?, ?, ?, ?, ?, ?)"
                    (vector engin-id chantier-id agence date-debut date-fin prix-total))
    (sqlite-close db)
    (message "Location ajoutée avec succès !")))




;; Fonction pour lister les locations existantes
(defun engins-liste-locations ()
  "Retourne la liste de toutes les locations."
  (let ((db (sqlite-open engins-db)))
    (let ((resultat (sqlite-select db \
                                   "SELECT l.id, e.type, e.reference, c.nom, l.agence, l.date_debut, l.date_fin, l.prix_total, l.statut \
                                    FROM locations l \
                                    JOIN engins e ON l.engin_id = e.id \
                                    JOIN chantiers c ON l.chantier_id = c.id")))
      (sqlite-close db)
      resultat)))



;;Interface utilisateur pour ajouter une location
(defun engins-ui-ajouter-location ()
  "Interface pour ajouter une nouvelle location."
  (let* ((chantiers (mapcar (lambda (c) (format "%d: %s" (nth 0 c) (nth 1 c)))
                            (engins-liste-chantiers)))
         (chantier-choice (completing-read "Chantier: " chantiers))
         (chantier-id (string-to-number (car (split-string chantier-choice ":"))))
         (engins (mapcar (lambda (e) (format "%d: %s (%s)" (nth 0 e) (nth 1 e) (nth 2 e)))
                         (engins-liste-par-chantier chantier-id)))
         (engin-choice (completing-read "Engin: " engins))
         (engin-id (string-to-number (car (split-string engin-choice ":"))))
         (agence (read-string "Agence: "))
         (date-debut (org-read-date nil nil nil "Date de début ?"))
         (date-fin (org-read-date nil nil nil "Date de fin ?")))
    (engins-ajouter-location engin-id chantier-id agence date-debut date-fin)))



;;;###autoload
(defun engins-setup ()
  "Configure l'application de gestion des engins."
  (interactive)
  (engins-init-db)
  (engins-ui))

;; Ajout d'une fonction de vérification de la base de données
;;;###autoload
(defun engins-verify-db ()
  "Vérifie l'état de la base de données et retourne les informations de débogage."
  (interactive)
  (let ((db (sqlite-open engins-db)))
    (message "Contenu de la table chantiers:")
    (message "%S" (sqlite-select db "SELECT * FROM chantiers"))
    (message "Contenu de la table engins:")
    (message "%S" (sqlite-select db "SELECT * FROM engins"))
    (sqlite-close db)))

;;; engins.el ends here
